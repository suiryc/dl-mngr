package suiryc.dl.mngr

import akka.actor.ActorRef
import com.typesafe.scalalogging.LazyLogging
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.attribute.FileTime
import java.nio.file.{FileAlreadyExistsException, Files, Path, StandardOpenOption}
import java.util.Date
import java.util.concurrent.locks.ReentrantReadWriteLock
import org.apache.http.nio.{ContentDecoder, ContentDecoderChannel, FileContentDecoder}
import scala.collection.JavaConverters._
import suiryc.dl.mngr.model.{DownloadException, DownloadInfo, SegmentRange, SegmentRanges}
import suiryc.scala.concurrent.locks.RichLock._
import suiryc.scala.io.{FileTimes, FilesEx, PathsEx}


object DownloadFile {

  def reuse(path: Path): DownloadFile = {
    reuse(path, Main.settings.getTemporaryFile(path))
  }

  def reuse(path: Path, temporary: Option[Path]): DownloadFile = {
    val downloadFile = new DownloadFile(path)
    downloadFile.temporary = temporary
    downloadFile.reused = true
    downloadFile
  }

}

class DownloadFile(private var path: Path) extends LazyLogging {

  /** Whether file is being reused. */
  private var reused: Boolean = false

  /** Whether file is to be truncated. */
  private var truncate: Boolean = false

  /** The temporary file to download into. */
  private var temporary = Main.settings.getTemporaryFile(path)

  /** The channel to write to. */
  private var channel: FileChannel = _

  /**
   * Pending (not yet 'force'd) ranges.
   * Note: there is no need to enforce the actual download length.
   */
  val pendingRanges = new SegmentRanges(Long.MaxValue, full = false)

  /** (Estimated) Size of pending data. */
  private var pending: Long = 0L

  /**
   * Lock to protect 'pending' data and prevent writing concurrently to
   * flushing.
   */
  private val lock = new ReentrantReadWriteLock()

  /** Target path (may be renamed). */
  def getPath: Path = path

  /** Temporary path. */
  def getTemporaryPath: Option[Path] = temporary

  /** Resets. */
  def reset(reusedOpt: Option[Boolean], truncate: Boolean): Unit = {
    close(None, done = false)
    reusedOpt.foreach(reused = _)
    this.truncate = truncate
  }

  /**
   * Gets currently downloaded ranges.
   *
   * Take into account remaining ranges and pending ones.
   */
  def getDownloadedRanges(info: DownloadInfo): List[SegmentRange] = {
    info.remainingRanges.map { remainingRanges ⇒
      // First consider everything was downloaded
      val downloaded = new SegmentRanges(remainingRanges.total)
      // Remove remaining ranges. Note: it is not mandatory to lock (which would
      // need to be also used when accessing the object inside FileDownloader).
      remainingRanges.getRanges.asScala.toList.foreach(downloaded.remove)
      // Remove pending ranges.
      lock.writeLock.withLock {
        pendingRanges.getRanges.asScala.toList.foreach(downloaded.remove)
      }
      downloaded.getRanges.asScala.toList
    }.getOrElse {
      // If we did not determine the size yet, but have already 'downloaded',
      // keep this as downloaded ranges. May happen when 'resuming' an existing
      // file (considering its size as already downloaded) and closing the app
      // before actually starting to download the file from its current state
      // (and thus not having remainingRanges populated yet).
      val downloaded = info.downloaded.get
      if (!info.isSizeDetermined && (downloaded > 0)) {
        List(SegmentRange(0, downloaded - 1))
      } else {
        Nil
      }
    }
  }

  /**
   * Creates the channel (if not already done).
   *
   * Must be called before before starting (writing).
   * Automatically uses another filename if file already exists (and was not
   * owned by us yet).
   *
   * @return whether channel was created (false if already done)
   */
  def createChannel(): Boolean = {
    if (channel == null) {
      val target = temporary.getOrElse(path)
      // If not owned, make sure the file does not exist upon creating it.
      val options = List(StandardOpenOption.CREATE, StandardOpenOption.WRITE) ++
        (if (!reused) List(StandardOpenOption.CREATE_NEW) else List.empty) ++
        (if (truncate) List(StandardOpenOption.TRUNCATE_EXISTING) else List.empty)

      try {
        channel = FileChannel.open(target, options: _*)
      } catch {
        case _: FileAlreadyExistsException ⇒
          // If the file already exists, find the next available name
          val available = PathsEx.getAvailable(target)
          channel = FileChannel.open(available, options: _*)
          if (temporary.isEmpty) path = available
          else temporary = Some(available)
      }
      // It's easier to force 'reused' to true now so that we can
      // stop/resume/restart without triggering renaming later.
      reused = true
      true
    } else false
  }

  // Notes:
  // Multiple response consumers may need to write to the file concurrently.
  // NIO is fine with it, but we need to take care of 'pending' data (written
  // but not yet 'force'd): upon 'write' we accumulate 'pending' data, and from
  // time to time those data are 'force'd and an I/O error may be triggered.
  // Upon such issue, the FileDownloader (which holds the consumers) needs to
  // 'undo' the failed ranges and mark them again as 'remaining'.
  //
  // Without proper locking/sequentiality, what may happen is then:
  //  1.1 Writer 1 calls 'write' successfully, had no need to 'force' and is
  //      sending 'Downloaded' to FileDownloader
  //  1.2 Writer 2 calls 'write' successfully, needs 'force' and triggers an
  //      I/O error, invalidating its writing (which was not yet announced) but
  //      also Writer 1 one
  //  2.1 Writer 2 propagates the I/O error to the FileDownloader by sending
  //      a message (concurrently to the 'Downloaded' one from Writer 1)
  //  2.2 Race condition: the failure message is received first, and the failed
  //      ranges are marked as 'remaining'
  //  2.3 The 'Downloaded' message from Writer 1 is received: FileDownloader
  //      now believes that the concerned range is done (while it was actually
  //      to be marked 'remaining' due to the failure)
  //
  // A solution is to void the race condition between sent messages, so that
  // 'Downloaded' is always sent first (and the failure can properly invalidate
  // it). To do so, we either need to send all messages from here, or at least
  // do so for the 'Downloaded' one (while holding an exclusive lock with the
  // code handling the possible I/O failure on 'force').
  //
  // Since 'flush' is also used upon 'close', it's easier to keep a thrown
  // DownloadException and let caller propagate/handle the issue.
  // To limit locking, we can use a ReadWriteLock with:
  //  - 'read' (shared) locking when writing to the file: so that writers don't
  //    block each other
  //  - 'write' (exclusive) locking when forcing data: so that no writing is
  //    being done at the same time
  // The 'pending' must be updated after writing while still holding the 'read'
  // lock, so that upon 'force' failure (holding exclusive lock) we can know
  // for sure which ranges are concerned.
  //
  // Unfortunately the Apache NIO FileContentDecoder implementation is not
  // thread-safe (uses channel position depending on situation).
  // So for now use an exclusive lock upon writing and flushing.
  // TODO: way to workaround this ? (then use 'shared' lock upon writing and
  // properly lock 'pending' between writing/flushing/getDownloadRanges).

  def write(contentDecoder: ContentDecoder, position: Long, count: Long, dler: ActorRef): Long = {
    // If the current channel size is lower than the write position, transfer
    // may either fail or do nothing. So ensure size is ok.
    // The easiest way, knowing we are about to write data at position, is to
    // write one dummy byte right now.
    if (channel.size < position) channel.write(ByteBuffer.wrap(Array[Byte](0x00)), position)

    // Use the 'shared' lock while writing: many callers can write at the same
    // time (NIO properly handle this), but 'force' cannot be used at the same
    // time ('exclusive' lock).
    val actual = lock.writeLock.withLock {
      val actual = contentDecoder match {
        case fcd: FileContentDecoder ⇒
          fcd.transfer(channel, position, count)

        case cd ⇒
          channel.transferFrom(new ContentDecoderChannel(cd), position, count)
      }

      // Send the 'Downloaded' message now to prevent race conditions with
      // possible I/O issue in 'force'.
      val range = SegmentRange(position, position + actual - 1)
      dler ! FileDownloader.Downloaded(range)

      // Update 'pending' data. Don't forget to use an exclusive lock since
      // those are not thread-safe.
      // Note: we cannot use 'lock.writeLock' as it is not permitted/possible
      // to upgrade from the 'shared' ('lock.readLock') to the 'exclusive' lock
      // with ReadWriteLock.
      //pendingRanges.synchronized {
        pendingRanges.add(range)
        pending += range.length
      //}

      range
    }

    // Flush when applicable
    flush(force = false)

    actual.length
  }

  def flush(force: Boolean): Unit = {
    // If flushing is needed, use the 'exclusive' lock: this way we are sure
    // there won't be any writing concurrently.
    // Note: we don't need to lock while checking the condition to flush (which
    // saves some time since most of them times we may not need to flush). But
    // once we do flush and need use the lock, check again to make sure only one
    // thread do it.
    @inline def needFlush: Boolean = force || (pending > Main.settings.bufferWriteFlushSize.get)
    if (needFlush) lock.writeLock.withLock {
      if (needFlush) {
        try {
          channel.force(force)
        } catch {
          case ex: Exception ⇒
            // Pass the failed ranges ('pending') to caller and let it handle it.
            // What is guaranteed is that 'Downloaded' messages for those ranges
            // have been sent already.
            DownloadException(
              message = s"I/O error: (${ex.getClass.getSimpleName}) ${ex.getMessage}",
              cause = ex,
              started = true,
              rangesWriteFailed = pendingRanges.getRanges.asScala.toList
            )
        } finally {
          // Reset 'pending' data. We are holding an exclusive lock, so it's fine
          // to update it directly.
          pendingRanges.clear()
          pending = 0
        }
      }
    }
    ()
  }

  def close(lastModified: Option[Date], done: Boolean): Unit = {
    if (channel != null) {
      if (channel.isOpen) {
        flush(force = true)
        channel.close()
      }
      channel = null
      lastModified.foreach { date ⇒
        // Change "last modified" time according to the server one.
        // Leave "creation" and "last access" as-is.
        val fileTime = FileTime.from(date.toInstant)
        val fileTimes = new FileTimes(
          creation = null,
          lastModified = fileTime,
          lastAccess = null
        )
        FilesEx.setTimes(temporary.getOrElse(path), fileTimes)
      }
      if (done) {
        temporary.foreach { tempPath ⇒
          // We only reuse the file we write to (temporary in this case).
          // If the target file exists, rename ours.
          val probed = PathsEx.getAvailable(path)
          if (probed != path) logger.warn(s"Path=<$path> already exists; saving to=<$probed> instead")
          path = probed
          Files.move(tempPath, path)
        }
      }
    }
  }

}
