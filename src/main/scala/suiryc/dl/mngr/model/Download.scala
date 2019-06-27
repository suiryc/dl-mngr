package suiryc.dl.mngr.model

import java.net.URI
import java.nio.file.Path
import java.time.LocalDateTime
import java.util.{Date, UUID}
import javafx.beans.property.{SimpleIntegerProperty, SimpleLongProperty, SimpleObjectProperty}
import javafx.collections.{FXCollections, ObservableList}
import scala.concurrent.Promise
import suiryc.dl.mngr.{DownloadFile, Main}

object DownloadState extends Enumeration {
  val Stopped, Pending, Running, Success, Failure = Value
}

object LogKind extends Enumeration {
  val Debug, Info, Warning, Error = Value
}

case class LogEntry(time: LocalDateTime, kind: LogKind.Value, message: String, exOpt: Option[Exception] = None)

case class NewDownloadInfo(
  /** Whether to automatically process this new download. */
  auto: Boolean = false,
  /** Remote URI to download from. */
  uri: Option[String] = None,
  /** Referrer URI. */
  referrer: Option[String] = None,
  /** Cookie. */
  cookie: Option[String] = None,
  /** User agent. */
  userAgent: Option[String] = None,
  /** Filename. */
  file: Option[String] = None,
  /** Size hint (informational). */
  sizeHint: Option[Long] = None,
  /** Comment. */
  comment: Option[String] = None
)

class DownloadInfo {
  // Note: some info changes need to be listened to (through Property).
  /** Promise completed once the download is 'finished' (success or failure). */
  var promise: Promise[Unit] = Promise()
  /** Remote URI to download from. */
  var uri: URI = _
  /** Actual (e.g. redirected) URI. */
  val actualUri: SimpleObjectProperty[URI] = new SimpleObjectProperty()
  /** File path. */
  val path: SimpleObjectProperty[Path] = new SimpleObjectProperty()
  /** File temporary path. */
  val temporaryPath: SimpleObjectProperty[Path] = new SimpleObjectProperty()
  /** State (initially stopped). */
  val state: SimpleObjectProperty[DownloadState.Value] = new SimpleObjectProperty(DownloadState.Stopped)
  /** Size. */
  val size: SimpleLongProperty = new SimpleLongProperty(Long.MinValue)
  /** File last modified time on server. */
  var lastModified: SimpleObjectProperty[Date] = new SimpleObjectProperty()
  /** Number of segments. */
  val segments: SimpleIntegerProperty = new SimpleIntegerProperty(0)
  /** Number of active segments. */
  val activeSegments: SimpleIntegerProperty = new SimpleIntegerProperty(0)
  /** Segments limit. */
  val maxSegments: SimpleIntegerProperty = new SimpleIntegerProperty(0)
  /** How many bytes are already downloaded. */
  val downloaded: SimpleLongProperty = new SimpleLongProperty(0)
  /**
   * Logs.
   *
   * Notes:
   * FXCollections are sensible to concurrent changes when dealing with added
   * elements: the exposed sublist wraps the original collection, and iterating
   * over it may throw a ConcurrentModificationException if the collection has
   * been changed since the exposed change.
   * This means the bare minimum is for both modifications and listeners to
   * either work in the same thread or synchronize themselves. The easiest
   * solution is to first synchronize 'addLog' (prevent concurrent modifications
   * on this end) and make sure listeners work on changes in the caller thread.
   */
  val logs: ObservableList[LogEntry] = FXCollections.observableArrayList()

  /** Whether download was active when download manager stopping was requested. */
  var wasActive: Boolean = false

  /** Remaining segment ranges. */
  var remainingRanges: Option[SegmentRanges] = None
  /** Range validator (if applicable). */
  var rangeValidator: Option[String] = None
  /** Whether server accept ranges. */
  var acceptRanges: SimpleObjectProperty[Option[Boolean]] = new SimpleObjectProperty(None)

  // Note: size is Long.MinValue before download actually starts,
  // and is -1 when started and size is unknown.
  def isSizeDetermined: Boolean = size.get != Long.MinValue
  def isSizeUnknown: Boolean = size.get == -1
  def isSizeKnown: Boolean = size.get >= 0

  def restart(): Unit = {
    remainingRanges = None
    rangeValidator = None
    acceptRanges.set(None)
    lastModified.set(null)
    downloaded.set(0)
  }

  def withLogs[A](f: ObservableList[LogEntry] => A): A = logs.synchronized {
    f(logs)
  }

  def addLog(kind: LogKind.Value, message: String, exOpt: Option[Exception] = None): Unit = logs.synchronized {
    val entry = LogEntry(time = LocalDateTime.now, kind = kind, message = message, exOpt = exOpt)
    logs.add(entry)
    ()
  }

}

/** Info to backup for a download. */
case class DownloadBackupInfo(
  /** Internal id. */
  id: UUID,
  /** Remote URI to download from. */
  uri: URI,
  /** Referrer URI. */
  referrer: Option[URI],
  /** Cookie. */
  cookie: Option[String],
  /** User agent. */
  userAgent: Option[String],
  /** Target save path. */
  path: Path,
  /** Temporary download path. */
  temporaryPath: Option[Path],
  /** Whether the download was done (finished with success). */
  done: Boolean,
  /** Whether the download can be resumed upon starting application. */
  canResume: Boolean,
  /** Download size. */
  size: Option[Long],
  /** Download size hint (informational). */
  sizeHint: Option[Long],
  /** Range validator (if applicable) */
  rangeValidator: Option[String],
  /** Whether server accept ranges */
  acceptRanges: Option[Boolean],
  /** File last modified time on server */
  lastModified: Option[Date],
  /** Downloaded ranges. */
  downloadedRanges: List[SegmentRange]
)

/**
 * A download properties and settings.
 *
 * Immutable values are in the class parameters.
 * Values that can be changed are in 'info'.
 */
case class Download(
  /** Internal id, for logging purposes. */
  id: UUID,
  /** Referrer URI. */
  referrer: Option[URI],
  /** Cookie. */
  cookie: Option[String],
  /** User agent. */
  userAgent: Option[String],
  /** Download file handler (I/O). */
  downloadFile: DownloadFile,
  /** Download size hint (informational). */
  sizeHint: Option[Long],
  /** Rate limiter. */
  rateLimiter: RateLimiter,
  /** Mutable info. */
  info: DownloadInfo = new DownloadInfo()
) {

  private var lastReason = Option.empty[String]

  def uri: URI = info.uri

  def setUri(uri: URI): Download = {
    info.uri = uri
    info.actualUri.set(uri)
    this
  }

  def path: Path = downloadFile.getPath

  def temporaryPath: Option[Path] = downloadFile.getTemporaryPath

  refreshPaths()

  private def refreshPaths(): Unit = {
    info.path.set(path)
    info.temporaryPath.set(temporaryPath.orNull)
  }

  def openFile(): Unit = {
    if (downloadFile.createChannel()) refreshPaths()
  }

  def setSize(size: Long): Unit = {
    // Don't bother if size is already set
    if (size != info.size.get) {
      info.size.set(size)
      // Pre-allocate space if applicable and requested.
      if ((size > 0) && Main.settings.preallocateEnabled.get) {
        val zero = Main.settings.preallocateZero.get
        if (downloadFile.preallocate(size, zero)) refreshPaths()
      }
    }
  }

  def renameFile(target: Path): Unit = {
    downloadFile.rename(target)
    refreshPaths()
  }

  def closeFile(lastModified: Option[Date], done: Boolean): Unit = {
    downloadFile.close(lastModified, done)
    if (done) refreshPaths()
  }

  def acceptRanges: Option[Boolean] = info.acceptRanges.get
  def acceptRanges(accept: Boolean): Unit = info.acceptRanges.set(Some(accept))

  def state: DownloadState.Value = info.state.getValue
  def isStarted: Boolean = info.isSizeDetermined || (info.downloaded.get > 0)
  def isStopped: Boolean = state == DownloadState.Stopped
  def isPending: Boolean = state == DownloadState.Pending
  def isRunning: Boolean = state == DownloadState.Running
  def isActive: Boolean = isRunning || isPending
  def isDone: Boolean = state == DownloadState.Success
  def isFailed: Boolean = state == DownloadState.Failure
  def canStop: Boolean = isActive
  def canResume(restart: Boolean): Boolean = if (restart) canRestart else canResume
  // We cannot resume if ranges are not supported.
  def canResume: Boolean = !acceptRanges.contains(false) && (isFailed || isStopped)
  // We can restart upon failure, or if stopped and ranges are not supported.
  def canRestart: Boolean = isFailed || (isStopped && acceptRanges.contains(false))

  def siteSettings: Main.settings.SiteSettings = Main.settings.getSite(info.actualUri.get)
  def activeSegments: Int = info.activeSegments.get
  def maxSegments: Int = siteSettings.getSegmentsMax
  def minSegmentSize: Long = Main.settings.segmentsMinSize.get

  def updateLastReason(opt: Option[String]): Unit = {
    if (opt != lastReason) {
      opt.foreach { reason =>
        info.addLog(LogKind.Info, reason)
      }
    }
    lastReason = opt
  }

  def context: String = s"[id=$id]"

  def context(range: SegmentRange): String = {
    if (range.isInfinite) s"$context[range=${range.start}-∞]"
    else if (range.length > 0) s"$context[range=${range.start}-${range.end}]"
    else context
  }

  def resume(reusedOpt: Option[Boolean], restart: Boolean): Unit = {
    if (restart) info.restart()
    val reason = if (restart) "re-started" else "resumed"
    downloadFile.reset(reusedOpt = reusedOpt, restart = restart)
    // Belt and suspenders:
    // The current download should have properly been failed/stopped already.
    // We still try to fail the promise, and in case it had not yet been
    // completed (and since we are resuming/restarting the download) make sure
    // (reused = true) the manager won't touch the download file.
    info.promise.tryFailure(DownloadException(s"Download $reason with another promise", reused = true))
    // Note: state will be changed through tryAcquireConnection
    info.promise = Promise[Unit]()
  }

  def backupInfo: DownloadBackupInfo = {
    DownloadBackupInfo(
      id = id,
      uri = uri,
      referrer = referrer,
      cookie = cookie,
      userAgent = userAgent,
      path = downloadFile.getPath,
      temporaryPath = downloadFile.getTemporaryPath,
      done = isDone,
      canResume = (isActive || info.wasActive) && !acceptRanges.contains(false),
      size = if (info.isSizeDetermined) Some(info.size.get) else None,
      sizeHint = sizeHint,
      rangeValidator = info.rangeValidator,
      acceptRanges = acceptRanges,
      lastModified = Option(info.lastModified.get),
      downloadedRanges = downloadFile.getDownloadedRanges(info)
    )
  }

}
