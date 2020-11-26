package suiryc.dl.mngr

import akka.actor.ActorRef
import com.typesafe.scalalogging.LazyLogging
import java.io.RandomAccessFile
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.attribute.FileTime
import java.nio.file._
import java.util.Date
import java.util.concurrent.locks.ReentrantReadWriteLock
import org.apache.http.nio.{ContentDecoder, ContentDecoderChannel, FileContentDecoder}
import scala.jdk.CollectionConverters._
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

  /** Whether download was done (success). */
  private var isDone: Boolean = false

  /** Whether file is being reused. */
  private var reused: Boolean = false

  /** Whether file is to be truncated. */
  private var truncate: Boolean = false

  /** The temporary file to download into. */
  private var temporary: Option[Path] = _
  renewTemporary()

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

  /** Working path (temporary or target). */
  def getWorkingPath: Path = temporary.getOrElse(path)

  private def renewTemporary(): Unit = temporary = Main.settings.getTemporaryFile(path)

  /** Resets. */
  def reset(reusedOpt: Option[Boolean], restart: Boolean): Unit = this.synchronized {
    // Notes:
    // We only allow to delete temporary path. Having created a file also means
    // we reserved its name, so don't delete the real target if we created it.
    // In this case, we simply need to truncate it when we will (re-)open it.
    close(None, done = false, canDelete = restart && temporary.isDefined)
    reusedOpt.foreach(reused = _)
    // We could also restrict truncating to 'without temporary path' cases, but
    // that is not necessary.
    this.truncate = restart
  }

  /**
   * Gets currently downloaded ranges.
   *
   * Take into account remaining ranges and pending ones.
   */
  def getDownloadedRanges(info: DownloadInfo): List[SegmentRange] = {
    info.remainingRanges.map { remainingRanges =>
      // First consider everything was downloaded
      val downloaded = new SegmentRanges(remainingRanges.total)
      // Remove remaining ranges. Note: it is not mandatory to lock (which would
      // need to be also used when accessing the object inside FileDownloader).
      remainingRanges.getRanges.asScala.toList.foreach(downloaded.remove)
      // Remove pending ranges: we *must not* consider those as downloaded since
      // flushing them may fail.
      // Note: here an exclusive lock is needed; 'lock.writeLock' is enough as
      // it prevents concurrent access from 'write' and 'flush'.
      lock.writeLock.withLock {
        pendingRanges.getRanges.asScala.toList.foreach(downloaded.remove)
      }
      downloaded.getRanges.asScala.toList
    }.getOrElse {
      // If we already have 'downloaded', keep it as downloaded range. This is
      // at least informational, and in some cases needed.
      // e.g. when 'resuming' an existing file (size considered as already
      // downloaded) and closing the app before actually starting to download
      // the file from its current state, the DL 'size' is not yet determined
      // and thus there is no remainingRanges populated yet.
      // This is also needed when 'size' is unknown while server accept ranges:
      // we can resume download starting from the downloaded size.
      val downloaded = info.downloaded.get
      if (downloaded > 0) {
        List(SegmentRange(0, downloaded - 1))
      } else {
        Nil
      }
    }
  }

  /**
   * Creates the channel (if not already done).
   *
   * Must be called before starting (writing).
   * Automatically uses another filename if file already exists (and was not
   * owned by us yet).
   *
   * @return whether channel was created (false if already done)
   */
  def createChannel(): Boolean = this.synchronized {
    if (channel == null) {
      // If we don't re-use an existing file, re-compute temporary path if any.
      // (useful if target was renamed in-between)
      if (!reused) renewTemporary()
      val target = getWorkingPath
      // If not owned, make sure the file does not exist upon creating it.
      val options = List(StandardOpenOption.CREATE, StandardOpenOption.WRITE) ++
        (if (!reused) List(StandardOpenOption.CREATE_NEW) else List.empty) ++
        (if (truncate) List(StandardOpenOption.TRUNCATE_EXISTING) else List.empty)

      try {
        channel = FileChannel.open(target, options: _*)
      } catch {
        case _: FileAlreadyExistsException | _: AccessDeniedException =>
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

  /**
   * Preallocates file size.
   *
   * If current file size is smaller than target, the file grows.
   * If requested, delta is filled with zeros.
   *
   * @param size target size
   * @param zero whether to fill space with zeros
   * @return whether channel was created (false if not done)
   */
  def preallocate(size: Long, zero: Boolean): Boolean = this.synchronized {
    // Notes:
    // Preallocating space by writing the very last byte works but does not
    // always appear to help enough in preventing/limiting fragmentation.
    // The best solution (for both Windows and Linux) is to set the file
    // length through 'RandomAccessFile.setLength' before writing anything.
    // Even opening the file and immediately writing zeros until target size is
    // reached may create more fragmentation (especially on Windows with 'small'
    // write arrays of 64KiB for example); so always fix length before.

    // Exclusive lock is needed (first connection running in the
    // background). Don't do anything if we were already done.
    if ((size > 0) && !isDone) lock.writeLock.withLock {
      // Create channel if necessary (usually already done when first connection
      // was acquired).
      val created = createChannel()
      val currentEnd = channel.size() - 1
      val targetEnd = size - 1
      if (currentEnd < targetEnd) {
        // File needs to grow. The best solution is to fix the length through
        // RandomAccessFile, which we need to create temporarily because there
        // is no way to get one from our FileChannel.
        // For the best result, caller must call us *before* writing anything.
        val raf = new RandomAccessFile(getWorkingPath.toFile, "rwd")
        raf.setLength(size)
        raf.close()

        if (zero) {
          // How many zeros to write at a time.
          // Starting from the current end (+ 1), we write aligned chunks of
          // zeros.
          val bufSize = 1024 * 1024
          val bb = ByteBuffer.wrap(Array.fill(bufSize)(0x00))
          @scala.annotation.tailrec
          def loop(pos: Long): Unit = {
            if (pos <= targetEnd) {
              // Compute how many zeros we need to write in order to fill the
              // chunk the writing position belongs to.
              // We determine:
              //  - how many bytes are already used (written) in the chunk
              val used = (pos % bufSize).toInt
              //  - how many bytes remain until target end
              val limit0 = math.min(Int.MaxValue, targetEnd + 1 - pos).toInt
              //  - how many bytes remain until end of chunk or target end
              val limit = math.min(bufSize - used, limit0)
              // Do the writing, and go on.
              bb.clear().limit(limit)
              loop(pos + channel.write(bb, pos))
            }
          }
          loop(currentEnd + 1)
          // Make sure write is completed.
          channel.force(false)
        }
      }
      created
    } else {
      false
    }
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
  // Since 'flush' is also used upon 'close', it's easier to throw a
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
  // thread-safe as it may use the channel current position.
  // So for now also use an exclusive lock upon writing.

  def write(contentDecoder: ContentDecoder, position: Long, count: Long, dler: ActorRef): Long = {
    // If the current channel size is lower than the write position, transfer
    // may either fail or do nothing. So ensure size is ok.
    // The easiest way, knowing we are about to write data at position, is to
    // write one dummy byte right now.
    if (channel.size < position) channel.write(ByteBuffer.wrap(Array[Byte](0x00)), position)

    // Use the 'shared' lock while writing: many callers can write at the same
    // time (NIO properly handle this), but 'force' cannot be used at the same
    // time ('exclusive' lock).
    // Note: as explained above, actually use the exclusive lock because Apache
    // NIO is not thread-safe.
    val actual = lock.writeLock.withLock {
      val actual = contentDecoder match {
        case fcd: FileContentDecoder =>
          fcd.transfer(channel, position, count)

        case cd =>
          channel.transferFrom(new ContentDecoderChannel(cd), position, count)
      }

      // Send the 'Downloaded' message now to prevent race conditions with
      // possible I/O issue in 'force'.
      val range = SegmentRange(position, position + actual - 1)
      dler ! FileDownloader.Downloaded(range)

      // Update 'pending' data.
      // Notes:
      // We need an exclusive lock because those are not thread-safe.
      // 'lock.writeLock' is fine, but we would not be able to use it if we were
      // already using 'lock.readLock' above as it is not permitted/possible to
      // upgrade from 'shared' to 'exclusive' lock with ReadWriteLock; in this
      // case the easiest solution would be to 'synchronized' this access.
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
    // saves some time since most of the time we may not need to flush). But
    // once we do need to flush and use the lock, check again to make sure only
    // one thread do it.
    @inline def needFlush: Boolean = force || (pending > Main.settings.bufferWriteFlushSize.get)
    if (needFlush) lock.writeLock.withLock {
      if (needFlush) {
        try {
          channel.force(force)
        } catch {
          case ex: Exception =>
            // Pass the failed ranges ('pending') to caller and let it handle it.
            // What is guaranteed is that 'Downloaded' messages for those ranges
            // have been sent already.
            // Note: make sure to build an immutable collection from the java
            // one since the latter will be cleared/reused.
            throw DownloadException(
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

  /**
   * Renames target path.
   *
   * Depending on the situation (download done, temporary file used, etc.), the
   * file may be renamed right now. If target path is already used, a new name
   * will be automatically chosen.
   *
   * @param target new target path
   * @return the new target path
   */
  def rename(target: Path): Path = this.synchronized {
    // Do nothing if target does not change.
    if (target != path) {
      if (isDone) {
        // If we were done the current 'path' is the real file.
        path = move(path, target)
      } else {
        // The download is not done. Is there a temporary path ?
        temporary match {
          case Some(tmp) =>
            // There is a temporary path, so we only need to set the target path.
            path = target
            if (target == tmp) {
              // The temporary path is now the actual target.
              temporary = None
            } else if ((channel == null) && !reused) {
              // We won't re-use the temporary path, which is not opened yet.
              // We can re-compute it now (may be displayed in UI).
              renewTemporary()
            }

          case None =>
            if ((channel == null) && reused) {
              // We will re-use the path, which is not opened yet. So we only
              // need to rename it now.
              path = move(path, target)
            } else {
              // Either file is opened, or we don't own it: we cannot safely
              // rename it, so we make it temporary and change the target.
              temporary = Some(path)
              path = target
            }
        }
      }
    }

    path
  }

  /**
   * Moves (or renames if possible) file.
   *
   * If target already exists, a new name will be automatically chosen.
   *
   * @param source source path
   * @param target target path
   * @return actual target path
   */
  private def move(source: Path, target: Path): Path = {
    @scala.annotation.tailrec
    def loop(remainingAttempts: Int): Path = {
      val probed = PathsEx.getAvailable(target)
      if (probed != target) logger.warn(s"Path=<$target> already exists; saving to=<$probed> instead")
      try {
        Files.move(source, probed)
        probed
      } catch {
        case ex: Exception =>
          if (remainingAttempts == 0) throw ex
          loop(remainingAttempts - 1)
      }
    }

    // If we are competing to use the target name, try more than once.
    loop(3)
  }

  def close(lastModified: Option[Date], done: Boolean, canDelete: Boolean = false): Unit = this.synchronized {
    isDone = done
    if (channel != null) {
      flush(force = true)
      // If not done, and file is empty (or caller allows deletion), delete it.
      val delete = !done && (canDelete || (channel.size == 0))
      channel.close()
      channel = null
      lastModified.foreach { date =>
        // Change "last modified" time according to the server one.
        // Leave "creation" and "last access" as-is.
        val fileTime = FileTime.from(date.toInstant)
        val fileTimes = new FileTimes(
          creation = null,
          lastModified = fileTime,
          lastAccess = null
        )
        FilesEx.setTimes(getWorkingPath, fileTimes)
      }
      if (done) {
        temporary.foreach { tempPath =>
          // We only reuse the file we write to (temporary in this case).
          // If the target file exists, rename ours.
          path = move(tempPath, path)
          // We are done with the temporary path.
          temporary = None
        }
      } else if (delete) {
        getWorkingPath.toFile.delete()
        ()
      }
    }
  }

}
