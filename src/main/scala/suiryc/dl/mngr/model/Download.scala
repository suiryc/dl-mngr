package suiryc.dl.mngr.model

import javafx.beans.property.{SimpleIntegerProperty, SimpleLongProperty, SimpleObjectProperty}
import spray.json._
import suiryc.dl.mngr.{DownloadFile, Main}
import suiryc.dl.mngr.I18N.Strings
import suiryc.dl.mngr.Main.Params
import suiryc.dl.mngr.util.Misc
import suiryc.scala.io.PathsEx
import suiryc.scala.spray.json.JsonFormats

import java.net.{Inet4Address, Inet6Address, InetAddress, URI}
import java.nio.file.Path
import java.util.{Date, UUID}
import scala.concurrent.Promise

object DownloadState extends Enumeration {
  // Stopped: download was not done nor failed, and stopped.
  // Pending: download is waiting for available connection(s).
  // Downloading: content is being downloaded.
  // Processing: content has been downloaded and is being processed.
  // Done: download is fully complete.
  // Failure: download failed before completion.
  val Stopped, Pending, Downloading, Processing, Done, Failure = Value
}

class DownloadInfo extends ObservableLogs {
  // Note: some info changes need to be listened to (through Property).
  /** Promise completed once the content download is 'finished' (success or failure). */
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
  /** Internet address. */
  val inetAddress: SimpleObjectProperty[Option[InetAddress]] = new SimpleObjectProperty(None)
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

  /** Whether content downloading is done. */
  var downloadDone: Boolean = false

  /** Done download error. */
  var doneError: Option[String] = None

  /** Whether download could be stopped when download manager stopping was requested. */
  var couldStop: Boolean = false

  /** Remaining segment ranges. */
  var remainingRanges: Option[SegmentRanges] = None
  /** Range validator (if applicable). */
  var rangeValidator: Option[String] = None
  /** Whether server accept ranges. */
  var acceptRanges: SimpleObjectProperty[Option[Boolean]] = new SimpleObjectProperty(None)

  /** Stream segments. */
  var streamSegments: List[StreamSegment] = Nil

  /** HLS. */
  var hls: Option[HLSInfo] = None

  /** Associated subtitles. */
  var subtitle: Option[SubtitleInfo] = None

  def isInetAddressDetermined: Boolean = inetAddress.get.nonEmpty
  def isIPv4: Boolean = inetAddress.get.exists(_.isInstanceOf[Inet4Address])
  def isIPv6: Boolean = inetAddress.get.exists(_.isInstanceOf[Inet6Address])

  // Note: size is Long.MinValue before download actually starts,
  // and is -1 when started and size is unknown.
  def isSizeDetermined: Boolean = size.get != Long.MinValue
  def isSizeUnknown: Boolean = size.get == -1
  def isSizeKnown: Boolean = size.get >= 0

  def hasDownloaded: Boolean = downloaded.get > 0

  def restart(): Unit = {
    remainingRanges = None
    rangeValidator = None
    acceptRanges.set(None)
    lastModified.set(null)
    size.set(Long.MinValue)
    downloaded.set(0)
    doneError = None
  }

}

case class DownloadBackupPathsInfo(
  /** Target save path. */
  target: Path,
  /** Whether the target path was created. */
  targetCreated: Boolean,
  /** Temporary download path. */
  temporary: Path,
  /** Whether the temporary path was created. */
  temporaryCreated: Boolean
)

case class DownloadBackupSizeInfo(
  /** Download size. */
  value: Option[Long],
  /** Download size hint (informational). */
  hint: Option[Long],
  /** Download size qualifier (informational). */
  qualifier: Option[String]
)

case class DownloadBackupRangesInfo(
  /** Range validator (if applicable) */
  validator: Option[String],
  /** Whether server accept ranges */
  accept: Option[Boolean],
  /** Downloaded ranges. */
  downloaded: List[SegmentRange]
)

/** Info to back up for a download. */
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
  /** HTTP headers. */
  headers: List[Params.Header],
  /** Paths information. */
  paths: DownloadBackupPathsInfo,
  /** Whether content downloading as done. */
  downloadDone: Boolean,
  /** Whether the download (content and processing) was done (finished with success or error). */
  done: Boolean,
  /** Done download error. */
  doneError: Option[String],
  /** Whether the download can be resumed upon starting application. */
  canResume: Boolean,
  /** Download size info. */
  size: DownloadBackupSizeInfo,
  /** Download ranges info. */
  ranges: DownloadBackupRangesInfo,
  /** File last modified time on server */
  lastModified: Option[Date],
  /** Streams segments. */
  streamSegments: List[StreamSegment],
  /** HLS. */
  hls: Option[HLSInfo],
  /** Subtitles. */
  subtitle: Option[SubtitleInfo]
)

object DownloadBackupInfo extends DefaultJsonProtocol with JsonFormats {
  implicit val segmentRangeFormat: RootJsonFormat[SegmentRange] = jsonFormat2(SegmentRange.apply)
  implicit val subtitlesBackupFormat: RootJsonFormat[SubtitleInfo] = jsonFormat3(SubtitleInfo.apply)
  implicit val downloadBackupPathsFormat: RootJsonFormat[DownloadBackupPathsInfo] = jsonFormat4(DownloadBackupPathsInfo.apply)
  implicit val downloadBackupSizeFormat: RootJsonFormat[DownloadBackupSizeInfo] = jsonFormat3(DownloadBackupSizeInfo.apply)
  implicit val downloadBackupRangesFormat: RootJsonFormat[DownloadBackupRangesInfo] = jsonFormat3(DownloadBackupRangesInfo.apply)
  implicit val downloadBackupFormat: RootJsonFormat[DownloadBackupInfo] = jsonFormat17(DownloadBackupInfo.apply)
}

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
  /** HTTP headers. */
  headers: List[Params.Header],
  /** Download file handler (I/O). */
  downloadFile: DownloadFile,
  /** Download size hint (informational). */
  sizeHint: Option[Long],
  /** Download size qualifier (informational). */
  sizeQualifier: Option[String],
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

  def setHLS(hls: Option[HLSInfo]): Download = {
    info.hls = hls
    this
  }

  def setSubtitle(subtitle: Option[SubtitleInfo]): Download = {
    info.subtitle = subtitle
    this
  }

  def path: Path = downloadFile.getPath

  def targetCreated: Boolean = downloadFile.getCreatedTarget

  def temporaryPath: Path = downloadFile.getTemporaryPath

  def temporaryCreated: Boolean = downloadFile.getCreated

  refreshPaths()

  private def refreshPaths(): Unit = {
    info.path.set(path)
    info.temporaryPath.set(temporaryPath)
  }

  /** Creates temporary directory path. */
  def createDirectory(): Unit = {
    // Belt and suspenders: we only expect to be called for HLS download.
    if (info.hls.nonEmpty && downloadFile.createDirectory()) {
      refreshPaths()
    }
  }

  /** Creates (reserves) target path. */
  def createTargetPath(): Unit = {
    downloadFile.createTarget()
    refreshPaths()
  }

  /** Opens target file. */
  def openFile(): Unit = {
    // Belt and suspenders: we only expect to be called for regular file
    // download, not HLS.
    if (info.hls.isEmpty && downloadFile.createChannel()) {
      refreshPaths()
    }
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
    // Determine current folder and name.
    val currentFolder = path.getParent
    val currentName = PathsEx.atomicName(path)
    // Rename download, and determine new folder and name.
    // (actual name may have been changed if requested one was not available)
    downloadFile.rename(target)
    val newFolder = path.getParent
    val newName = PathsEx.atomicName(path)

    // Rename subtitle file when applicable: we must have already saved
    // subtitles in a file (filename known).
    info.subtitle.foreach { subtitle =>
      subtitle.filename.foreach { subtitleFilename =>
        // And new download folder/name should have changed.
        if ((newName != currentName) || (newFolder != currentFolder)) {
          // We re-determine the subtitle filename from actual download. This is
          // useful if original subtitle filename was renamed if not available:
          // we get a chance to use the preferred filename if available.
          val actualPath = Misc.moveFile(
            currentFolder.resolve(subtitleFilename),
            subtitle.determinePath(this),
            dot = true
          )
          // Remember new filename.
          setSubtitle(Some(subtitle.copy(
            filename = Some(actualPath.getFileName.toString)
          )))
        }
      }
    }

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
  def isDownloading: Boolean = state == DownloadState.Downloading
  def isProcessing: Boolean = state == DownloadState.Processing
  // Download is 'busy' when it is doing something, which e.g. requires
  // confirmation and action to stop.
  def isBusy: Boolean = isDownloading || isProcessing
  // Download is using or waiting for network connection(s).
  def isNetworkActive: Boolean = isDownloading || isPending
  def isDone: Boolean = state == DownloadState.Done
  def isFailed: Boolean = state == DownloadState.Failure
  // Download 'can be stopped' when it is doing something or waiting to.
  def canStop: Boolean = isNetworkActive || isBusy
  def canResume(restart: Boolean): Boolean = if (restart) canRestart else canResume
  // We cannot resume if ranges are not supported.
  def canResume: Boolean = (info.downloadDone || !acceptRanges.contains(false)) && (isFailed || isStopped)
  // We can restart upon failure, if stopped and ranges are not supported, or if done with error.
  def canRestart: Boolean = isFailed || (isStopped && acceptRanges.contains(false)) || (isDone && doneError.nonEmpty)

  def doneError: Option[String] = info.doneError

  def siteSettings: Main.settings.SiteSettings = Main.settings.findSite(info.actualUri.get)
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
    if (range.length > 0) s"$context[range=${range.start}-${range.endStr}]"
    else context
  }

  def ipContext: String = info.inetAddress.get.map { addr =>
    s" ${if (info.isIPv6) "ipv6" else s"ipv4"}=<${addr.getHostAddress}>"
  }.getOrElse("")

  def fileContext: String = Misc.fileContext(path)

  def tooltip: String = {
    List(
      s"${Strings.fileColon} $path",
      if (siteSettings.isDefault) s"${Strings.serverColon} ${uri.getHost}"
      else s"${Strings.siteColon} ${siteSettings.site}"
    ).mkString("\n")
  }

  def resume(restart: Boolean): Unit = {
    if (restart) info.restart()
    val reason = if (restart) "re-started" else "resumed"
    downloadFile.reset(restart = restart)
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
      headers = headers,
      paths = DownloadBackupPathsInfo(
        target = downloadFile.getPath,
        targetCreated = targetCreated,
        temporary = temporaryPath,
        temporaryCreated = temporaryCreated
      ),
      downloadDone = info.downloadDone,
      done = isDone,
      doneError = doneError,
      canResume = (canStop || info.couldStop) && !acceptRanges.contains(false),
      size = DownloadBackupSizeInfo(
        value = if (info.isSizeDetermined) Some(info.size.get) else None,
        hint = sizeHint,
        qualifier = sizeQualifier
      ),
      ranges = DownloadBackupRangesInfo(
        validator = info.rangeValidator,
        accept = acceptRanges,
        downloaded = downloadFile.getDownloadedRanges(info)
      ),
      lastModified = Option(info.lastModified.get),
      streamSegments = info.streamSegments,
      hls = info.hls,
      subtitle = info.subtitle
    )
  }

}
