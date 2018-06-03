package suiryc.dl.mngr.model

import java.net.URI
import java.nio.file.Path
import java.time.LocalDateTime
import java.util.{Date, UUID}
import javafx.beans.property.{SimpleIntegerProperty, SimpleLongProperty, SimpleObjectProperty}
import javafx.collections.{FXCollections, ObservableList}
import scala.concurrent.Promise
import suiryc.dl.mngr.{DownloadFile, Main, Settings}

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
  /** Comment. */
  comment: Option[String] = None
)

class DownloadInfo {
  /** File path */
  val path: SimpleObjectProperty[Path] = new SimpleObjectProperty()
  /** State (initially stopped) */
  val state: SimpleObjectProperty[DownloadState.Value] = new SimpleObjectProperty(DownloadState.Stopped)
  /** Size */
  val size: SimpleLongProperty = new SimpleLongProperty(Long.MinValue)
  /** Number of active segments */
  val activeSegments: SimpleIntegerProperty = new SimpleIntegerProperty(0)
  /** Segments limit. */
  val maxSegments: SimpleIntegerProperty = new SimpleIntegerProperty(0)
  /** How many bytes are already downloaded. */
  val downloaded: SimpleLongProperty = new SimpleLongProperty(0)
  /** Logs */
  val logs: ObservableList[LogEntry] = FXCollections.observableArrayList()

  /** Whether download was active when downloader manager stopping was requested. */
  var wasActive: Boolean = false

  /** Remaining segment ranges */
  var remainingRanges: Option[SegmentRanges] = None
  /** Range validator (if applicable) */
  var rangeValidator: Option[String] = None
  /** Whether server accept ranges */
  var acceptRanges: Option[Boolean] = None
  /** File last modified time on server */
  var lastModified: Option[Date] = None

  // Note: size is Long.MinValue before download actually starts,
  // and is -1 when started and size is unknown.
  def isSizeDetermined: Boolean = size.get != Long.MinValue
  def isSizeUnknown: Boolean = size.get == -1

  def restart(): Unit = {
    remainingRanges = None
    rangeValidator = None
    acceptRanges = None
    lastModified = None
  }

  def addLog(kind: LogKind.Value, message: String, exOpt: Option[Exception] = None): Unit = {
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
  /** Range validator (if applicable) */
  rangeValidator: Option[String],
  /** Whether server accept ranges */
  acceptRanges: Option[Boolean],
  /** File last modified time on server */
  lastModified: Option[Date],
  /** Downloaded ranges. */
  downloadedRanges: List[SegmentRange]
)

/** A download properties and settings. */
case class Download(
  /** Internal id, for logging purposes. */
  id: UUID,
  /** Remote URI to download from. */
  uri: URI,
  /** Referrer URI. */
  referrer: Option[URI],
  /** Cookie. */
  cookie: Option[String],
  /** User agent. */
  userAgent: Option[String],
  /** Download file handler (I/O). */
  downloadFile: DownloadFile,
  /** Rate limiter. */
  rateLimiter: RateLimiter,
  /** Promise completed once the download is 'finished' (success or failure). */
  promise: Promise[Unit] = Promise(),
  /** Info */
  info: DownloadInfo = new DownloadInfo()
) {

  private var lastReason = Option.empty[String]

  def path: Path = downloadFile.getPath
  info.path.set(path)

  def openFile(): Unit = {
    if (downloadFile.createChannel()) info.path.set(path)
  }

  def closeFile(lastModified: Option[Date], done: Boolean): Unit = {
    downloadFile.close(lastModified, done)
    if (done) info.path.set(path)
  }

  def state: DownloadState.Value = info.state.getValue
  def isStarted: Boolean = info.isSizeDetermined || (info.downloaded.get > 0)
  def isRunning: Boolean = state == DownloadState.Running
  def isActive: Boolean = (state == DownloadState.Running) || (state == DownloadState.Pending)
  def isDone: Boolean = state == DownloadState.Success
  def canStop: Boolean = isActive
  def canResume(restart: Boolean): Boolean = if (restart) canRestart else canResume
  def canResume: Boolean = (state == DownloadState.Failure) || (state == DownloadState.Stopped)
  def canRestart: Boolean = state == DownloadState.Failure

  def siteSettings: Settings#SiteSettings = {
    val sites = Main.settings.getSites
    @scala.annotation.tailrec
    def loop(host: String): Settings#SiteSettings = {
      // Site name must not be TLD alone
      val els = host.split("\\.", 2)
      if (els.size > 1) {
        // Check whether this host is known, otherwise go up one level
        sites.get(host) match {
          case Some(settings) ⇒ settings
          case None ⇒ loop(els(1))
        }
      } else {
        // Fallback to default
        Main.settings.sitesDefault
      }
    }
    loop(uri.getHost.toLowerCase)
  }

  def activeSegments: Int = info.activeSegments.get
  def maxSegments: Int = siteSettings.getSegmentsMax
  def minSegmentSize: Long = Main.settings.segmentsMinSize.get

  def updateLastReason(opt: Option[String]): Unit = {
    if (opt != lastReason) {
      opt.foreach { reason ⇒
        info.addLog(LogKind.Info, reason)
      }
    }
    lastReason = opt
  }

  def context: String = s"[id=$id]"

  def context(range: SegmentRange): String = {
    if (range.length > 0) s"$context[range=${range.start}-${range.end}]"
    else context
  }

  def resume(reusedOpt: Option[Boolean], restart: Boolean): Download = {
    if (restart) info.restart()
    val reason = if (restart) "re-started" else "resumed"
    // Make sure that previous resources cannot be used anymore
    closeFile(None, done = false)
    downloadFile.reset(reusedOpt = reusedOpt, truncate = restart)
    // Belt and suspenders:
    // The current download should have properly been failed/stopped already.
    // We still try to fail the promise, and in case it had not yet been
    // completed (and since we are resuming/restarting the download) make sure
    // (reused = true) the manager won't touch the download file.
    promise.tryFailure(DownloadException(s"Download $reason with another promise", reused = true))
    // Note: state will be changed through tryAcquireConnection
    copy(promise = Promise[Unit]())
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
      canResume = isActive || info.wasActive,
      size = if (info.isSizeDetermined) Some(info.size.get) else None,
      rangeValidator = info.rangeValidator,
      acceptRanges = info.acceptRanges,
      lastModified = info.lastModified,
      downloadedRanges = downloadFile.getDownloadedRanges(info)
    )
  }

}
