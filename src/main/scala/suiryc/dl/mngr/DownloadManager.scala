package suiryc.dl.mngr

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.UUID
import monix.execution.Cancelable
import org.apache.http.config.RegistryBuilder
import org.apache.http.conn.ssl.{NoopHostnameVerifier, TrustAllStrategy}
import org.apache.http.impl.client.DefaultRedirectStrategy
import org.apache.http.impl.nio.client.{CloseableHttpAsyncClient, HttpAsyncClients}
import org.apache.http.impl.nio.conn.PoolingNHttpClientConnectionManager
import org.apache.http.impl.nio.reactor.DefaultConnectingIOReactor
import org.apache.http.impl.nio.reactor.IOReactorConfig
import org.apache.http.nio.conn.{NoopIOSessionStrategy, SchemeIOSessionStrategy}
import org.apache.http.nio.conn.ssl.SSLIOSessionStrategy
import org.apache.http.ssl.SSLContextBuilder
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.util.Try
import suiryc.dl.mngr.model._
import suiryc.dl.mngr.util.Http
import suiryc.scala.io.PathsEx


object DownloadManager {

  // Notes:
  // A connection manager can be explicitly set to a client.
  // Many parameters (maximum number of connections, SSL strategy etc.) actually
  // are connection manager ones. When set on the client, they are actually not
  // applied if a connection manager is also set (if no connection manager is
  // set, a default one is created with the concerned parameters if any).
  //
  // A connection manager can be used by more than one client, in which case
  // only one of them is supposed to own it, while the others are required to
  // call 'setConnectionManagerShared(true)'. The manager and/or owning client
  // is also supposed to only be freed *after* all other clients are done.
  //
  // The client, when owning the connection manager, creates a thread for the
  // manager to work.
  //
  // To free resources, there are two possibilities:
  //  - close the client (owning the connection manager): this shutdowns the
  //    connection manager, with which the working thread finishes, and the
  //    client waits for the thread to end
  // or
  //  - shutdown the connection manager: the working thread will finish, and
  //    we are then expected to never use the client again

  class LazyClient(trustAll: Boolean) extends StrictLogging {

    private var started = false

    // TODO: there is no sure way to configure the read buffer (socket + apache)
    // ConnectionConfig has buffer size, but its the starting value which is
    // automatically expanded (x2) upon filling when needed ...
    // By forcing the buffer filling (wait upon writing), it still seems than no
    // more than ~128KiB (by default) of data are buffered, but how does this happen ?
    // IOReactorConfig can have rcv buffer size, but
    //  - it only restrict the socket buffer
    //  - with small size it seems respected
    //  - with higher size, forcing buffer filling, usually the amount of data ready is
    //    x2 or x3 (again, how does this happen ?)
    // However, setting a 'low' value drastically reduces the download speed.
    /** HTTP connection manager. */
    lazy private val connManager = {
      val bufferReadMax = Main.settings.bufferReadMax.get
      val bufSize = if (bufferReadMax > 0) {
        math.max(Main.settings.bufferReadMin.get, bufferReadMax)
      } else {
        0
      }
      val iorConfig = IOReactorConfig.custom
        .setRcvBufSize(bufSize.toInt)
        .build
      if (trustAll) {
        val sslStrategy = new SSLIOSessionStrategy(
          new SSLContextBuilder().loadTrustMaterial(new TrustAllStrategy()).build,
          null,
          null,
          new NoopHostnameVerifier()
        )
        val iosessionFactoryRegistry = RegistryBuilder.create[SchemeIOSessionStrategy]
          .register("http", NoopIOSessionStrategy.INSTANCE)
          .register("https", sslStrategy)
          .build
        new PoolingNHttpClientConnectionManager(new DefaultConnectingIOReactor(iorConfig), iosessionFactoryRegistry)
      } else {
        new PoolingNHttpClientConnectionManager(new DefaultConnectingIOReactor(iorConfig))
      }
    }
    // We handle the connection limits ourself
    // (default maximum connections are 2 for route/host, 20 in total)
    connManager.setDefaultMaxPerRoute(Int.MaxValue)
    connManager.setMaxTotal(Int.MaxValue)
    //val connConfig = ConnectionConfig.custom
    //    .setBufferSize(1)
    //    .build
    //connManager.setDefaultConnectionConfig(connConfig)
    /** HTTP client. */
    lazy private val client = HttpAsyncClients.custom
      .setConnectionManager(connManager)
      .setConnectionManagerShared(false)
      .setRedirectStrategy(new RelaxedRedirectStrategy)
      .build

    def isStarted: Boolean = started

    def getConnectionManager: PoolingNHttpClientConnectionManager = connManager

    def getClient: CloseableHttpAsyncClient = this.synchronized {
      started = true
      client.start()
      client
    }

    def close(): Unit = {
      if (isStarted) {
        try {
          connManager.shutdown()
        } catch {
          case ex: Exception ⇒
            logger.error(s"Failed to shutdown HTTP connection manager: ${ex.getMessage}", ex)
        }
        try {
          client.close()
        } catch {
          case ex: Exception ⇒
            logger.error(s"Failed to close HTTP client: ${ex.getMessage}", ex)
        }
      }
    }

  }

  class RelaxedRedirectStrategy extends DefaultRedirectStrategy {

    override protected def createLocationURI(location: String): URI = {
      try {
        super.createLocationURI(location)
      } catch {
        case ex: Exception ⇒
          // Sometimes the location may have invalid characters.
          // Use out own method in this case.
          try {
            Http.getURI(location)
          } catch {
            case _: Exception ⇒
              // Throw the original error if we also fail.
              throw ex
          }
      }
    }

  }

  /** Download entry (as handled by manager). */
  case class DownloadEntry(
    /** Download. */
    download: Download,
    /** Actor handling the download. */
    dler: ActorRef
  ) {
    def withDownload(download: Download): DownloadEntry = copy(download = download)
  }

}

class DownloadManager extends StrictLogging {

  import Main.Akka._
  import DownloadManager._

  /** Whether we are currently stopping (and waiting for downloads to properly stop). */
  @volatile
  private var stopping: Boolean = false

  /** Promise completed once manager is fully stopped. */
  private val stopped: Promise[Unit] = Promise()

  /** Download entries. */
  private var dlEntries: List[DownloadEntry] = Nil

  /** HTTP client. */
  private var client: LazyClient = _
  /** HTTP 'trustAll' client. */
  private var clientTrustAll: LazyClient = _

  /** Rate limiter (unlimited by default). */
  private val rateLimiter: RateLimiter = new RateLimiter(0)

  /** Total number of connection. */
  private var cnxTotal = 0L

  /** Number of connections per site. */
  private var cnxPerSite: Map[String, Long] = Map.empty.withDefaultValue(0L)

  /** Number of connections per server. */
  private var cnxPerServer: Map[String, Long] = Map.empty.withDefaultValue(0L)

  // Janitoring is done in a dedicated actor.
  private val janitor = system.actorOf(Props(new DownloadsJanitor(this)))

  setClient()

  def setClient(): Unit = {
    client = new LazyClient(trustAll = false)
    clientTrustAll = new LazyClient(trustAll = true)

    janitor ! DownloadsJanitor.Janitor(List(client, clientTrustAll))
  }

  def getClient(trustAll: Boolean): CloseableHttpAsyncClient = {
    if (trustAll) clientTrustAll.getClient else client.getClient
  }

  /** Changes rate limit. */
  def setRateLimit(bytesPerSecond: Long): Unit = this.synchronized {
    // Only apply if limit changed
    if (rateLimiter.bytesPerSecond != math.max(0, bytesPerSecond)) {
      rateLimiter.setBytesPerSecond(bytesPerSecond)
    }
  }

  /** Gets downloads. */
  def getDownloads: List[Download] = dlEntries.map(_.download)

  /** Gets download. */
  def getDownload(id: UUID): Option[Download] = getDownloadEntry(id).map(_.download)

  /** Gets download entry. */
  private def getDownloadEntry(id: UUID): Option[DownloadEntry] = dlEntries.find(_.download.id == id)

  def reorderDownloads(ordered: List[UUID]): Unit = this.synchronized {
    // Create a map to access downloads more easily
    val known = dlEntries.map { dlEntry ⇒ dlEntry.download.id → dlEntry }.toMap
    // Belt and suspenders: determine if 'ordered' is missing known downloads
    // May happen with race conditions: download created here but not yet added
    // in UI while reordering happened.
    val orderedSet = ordered.toSet
    val rest = dlEntries.map(_.download.id).filterNot(orderedSet.contains)

    def reorder(l: List[UUID]): List[DownloadEntry] = {
      l.foldLeft(List.empty[DownloadEntry]) { (acc, id) ⇒
        acc ::: known.get(id).toList
      }
    }
    // Use requested reordering, and append any missing download
    dlEntries = reorder(ordered) ::: reorder(rest)
  }

  def addDownload(uri: URI, referrer: Option[URI], cookie: Option[String],
    userAgent: Option[String], save: Path, sizeHint: Option[Long],
    reused: Boolean, insertFirst: Boolean): Download =
  {
    // Note: caller still has to 'resume' (start) the download.
    // Code being shared with real resuming, we don't open the target file here,
    // and let 'resumeDownload' follow the promise.
    val downloadFile = if (reused) {
      DownloadFile.reuse(save)
    } else {
      new DownloadFile(save)
    }
    val download = Download(
      id = UUID.randomUUID,
      uri = uri,
      referrer = referrer,
      cookie = cookie,
      userAgent = userAgent,
      downloadFile = downloadFile,
      sizeHint = sizeHint,
      rateLimiter = rateLimiter
    )
    if (reused) {
      // When resuming a file, consider its current length as already downloaded.
      val length = math.max(0, downloadFile.getTemporaryPath.getOrElse(downloadFile.getPath).toFile.length)
      download.info.downloaded.set(length)
    }
    addDownload(download, insertFirst)
  }

  def addDownload(download: Download, insertFirst: Boolean): Download = {
    val dler = system.actorOf(Props(new FileDownloader(dlMngr = this, dl = download)))
    val dlEntry = DownloadEntry(download = download, dler = dler)
    if (download.isDone) {
      logger.info(s"${download.context} Download uri=<${download.uri}> referrer=<${download.referrer.getOrElse("")}> file=<${download.path}> done")
      download.info.addLog(LogKind.Info, s"Download file=<${download.path}> available")
    } else {
      logger.info(s"${download.context} Download uri=<${download.uri}> referrer=<${download.referrer.getOrElse("")}> file=<${download.path}> ready")
      download.info.addLog(LogKind.Info, s"Download file=<${download.path}> ready")
    }
    if (insertFirst) dlEntries ::= dlEntry
    else dlEntries :+= dlEntry
    download
  }

  def findDownload(uri: URI): Option[Download] = {
    dlEntries.find { dlEntry ⇒
      // Check both original and actual URIs
      (dlEntry.download.uri == uri) || (dlEntry.download.info.uri.get == uri)
    }.map(_.download)
  }

  def stopDownload(id: UUID): Unit = {
    getDownloadEntry(id).foreach(stopDownload)
  }

  private def stopDownload(dlEntry: DownloadEntry): Unit = {
    if (dlEntry.download.canStop) dlEntry.dler ! FileDownloader.DownloadStop
  }

  def resumeDownload(id: UUID, reusedOpt: Option[Boolean], restart: Boolean, force: Boolean = false): Unit = {
    if (!stopping) {
      getDownloadEntry(id).foreach { dlEntry ⇒
        if (dlEntry.download.canResume(restart)) {
          val download = updateDownloadEntry(dlEntry.withDownload(dlEntry.download.resume(reusedOpt = reusedOpt, restart = restart))).download
          followDownload(download)
          dlEntry.dler ! FileDownloader.DownloadResume(download, restart = restart, force = force)
        }
      }
    }
  }

  def addDownloadConnection(id: UUID): Unit = {
    if (!stopping) {
      getDownloadEntry(id).foreach { dlEntry ⇒
        if (dlEntry.download.isActive) {
          dlEntry.dler ! FileDownloader.AddConnection
        } else if (dlEntry.download.canResume) {
          resumeDownload(id, reusedOpt = None, restart = false, force = true)
        }
      }
    }
  }

  def removeDownloadConnection(id: UUID): Unit = {
    if (!stopping) {
      getDownloadEntry(id).foreach { dlEntry ⇒
        if (dlEntry.download.isRunning) {
          dlEntry.dler ! FileDownloader.RemoveConnection
        }
      }
    }
  }

  def removeDownload(id: UUID): Unit = this.synchronized {
    getDownloadEntry(id).foreach { dlEntry ⇒
      if (dlEntry.download.isActive) {
        throw new Exception("Cannot remove download which is active")
      }
      dlEntries = dlEntries.filterNot(_.download.id == id)
    }
  }

  private def downloadDone(id: UUID, result: Try[Unit]): Unit = this.synchronized {
    // We should find the entry.
    getDownloadEntry(id) match {
      case Some(dlEntry) ⇒
        val download = dlEntry.download
        val failureOpt = result.failed.toOption.map {
          case ex: DownloadException ⇒ ex
          case ex: Exception ⇒ DownloadException(message = ex.getMessage, cause = ex)
        }
        // Belt and suspenders:
        // If the failure is due to the download being resumed/restarted,
        // *DO NOT* touch the download file nor change its state.
        // This should not happen though, as we are supposed to only be able
        // to resume/restart *after* the download has been 'done'.
        if (failureOpt.exists(_.reused)) {
          logger.error(s"${download.context} Download uri=<${download.uri}> was not done before being resumed/restarted")
          download.info.addLog(LogKind.Error, "Download was not done before being resumed/restarted")
        } else {
          val state = failureOpt match {
            case Some(ex) ⇒
              if (ex.stopped) DownloadState.Stopped
              else DownloadState.Failure

            case None ⇒
              DownloadState.Success
          }
          download.info.state.setValue(state)
          // Now that the state changed, give other downloads a chance to start and/or
          // get another connection.
          tryConnection(Some(id))
          checkDone()
          // When a download is done, it is also a good time to (force) save
          // our state.
          if (!stopping) saveState()
        }

      case None ⇒
        logger.error(s"Could not properly end missing download entry id=<$id>. Result: $result")
    }
  }

  def refreshDownloads(): Unit = {
    dlEntries.foreach { dlEntry ⇒
      dlEntry.dler ! FileDownloader.Refresh
    }
  }

  def tryConnection(sid: Option[UUID] = None): Unit = {
    def loop(dls: List[DownloadEntry]): Unit = {
      if (dls.nonEmpty) {
        val head = dls.head
        val tail = dls.tail
        val promise = Promise[Unit]()
        if (sid.contains(head.download.id) || !head.download.isActive) promise.trySuccess(())
        else head.dler ! FileDownloader.TryConnection(promise)
        promise.future.onComplete { _ ⇒
          loop(tail)
        }
      }
    }

    if (canTryAcquireConnection) loop(dlEntries)
  }

  private def followDownload(download: Download): Unit = {
    // Note: we use the download id because the actual download maye have been
    // updated since we started following it.
    download.promise.future.onComplete(r ⇒ downloadDone(download.id, r))
  }

  // Beware that caller must have an up-to-date download to update
  private def updateDownloadEntry(dlEntry: DownloadEntry): DownloadEntry = this.synchronized {
    case class Updated(found: Boolean = false, dlEntries: List[DownloadEntry] = Nil) {
      def add(dlEntry0: DownloadEntry): Updated = {
        val found0 = dlEntry0.download.id == dlEntry.download.id
        copy(
          found = found | found0,
          dlEntries = this.dlEntries :+ (if (found0) dlEntry else dlEntry0)
        )
      }
      def complete(): List[DownloadEntry] = {
        if (found) dlEntries
        else dlEntries :+ dlEntry
      }
    }

    val updated = dlEntries.foldLeft(Updated()) { (updated, dlEntry0) ⇒
      updated.add(dlEntry0)
    }
    dlEntries = updated.complete()
    dlEntry
  }

  def stop(): Future[Unit] = this.synchronized {
    stopping = true
    dlEntries.foreach { dlEntry ⇒
      dlEntry.download.info.wasActive = dlEntry.download.isActive
      stopDownload(dlEntry)
    }
    checkDone()
    stopped.future
  }

  private def checkDone(): Unit = {
    if (stopping && dlEntries.forall(!_.download.isRunning)) {
      janitor ! DownloadsJanitor.Stop
      // Note: even though clients will be freed by janitor, we still want to
      // wait for them to finish.
      client.close()
      clientTrustAll.close()
      stopped.trySuccess(())
    }
    ()
  }

  def saveState(): Unit = this.synchronized {
    // First backup current file if any
    val backupPath = PathsEx.backupPath(Main.statePath)
    if (Main.statePath.toFile.exists()) {
      backupPath.toFile.delete()
      Main.statePath.toFile.renameTo(backupPath.toFile)
    }

    // Prepare state to save
    val downloads = dlEntries.map(_.download.backupInfo)
    import spray.json._
    import JsonImplicits._
    val downloadsJson = downloads.toJson
    val stateJson = JsObject("downloads" → downloadsJson)
    val state = stateJson.prettyPrint

    // Save state
    Files.write(Main.statePath, state.getBytes(StandardCharsets.UTF_8))
    // Delete backup if there were no issue
    backupPath.toFile.delete()
    ()
  }

  def start(): Unit = {
    import spray.json._
    import JsonImplicits._

    def restoreState(downloadsBackupInfo: List[DownloadBackupInfo]): Unit = {
      downloadsBackupInfo.foreach { downloadBackupInfo ⇒
        val downloadFile = DownloadFile.reuse(downloadBackupInfo.path, downloadBackupInfo.temporaryPath)
        val download = Download(
          id = downloadBackupInfo.id,
          uri = downloadBackupInfo.uri,
          referrer = downloadBackupInfo.referrer,
          cookie = downloadBackupInfo.cookie,
          userAgent = downloadBackupInfo.userAgent,
          downloadFile = downloadFile,
          sizeHint = downloadBackupInfo.sizeHint,
          rateLimiter = rateLimiter
        )
        val info = download.info
        if (downloadBackupInfo.done) info.state.set(DownloadState.Success)
        val remainingRanges = downloadBackupInfo.size.map { size ⇒
          info.size.set(size)
          val remainingRanges = new SegmentRanges(size)
          downloadBackupInfo.downloadedRanges.foreach(remainingRanges.remove)
          remainingRanges
        }
        info.remainingRanges = remainingRanges
        info.rangeValidator = downloadBackupInfo.rangeValidator
        info.acceptRanges.set(downloadBackupInfo.acceptRanges)
        info.lastModified = downloadBackupInfo.lastModified
        // Handle case where downloadRanges is non-empty yet remainingRanges is.
        // e.g. restoring a file that was 'resumed' ('downloaded' known) but
        // actually not started again before closing the app.
        info.downloaded.set(downloadBackupInfo.downloadedRanges.map(_.length).sum)
        // In any case, if remainingRanges is known, rely on it.
        info.remainingRanges.foreach { remainingRanges ⇒
          info.downloaded.set(remainingRanges.getRemovedLength)
        }
        addDownload(download, insertFirst = false)
        if (downloadBackupInfo.canResume) resumeDownload(download.id, reusedOpt = None, restart = false)
      }
    }

    def readFile(path: Path): Boolean = {
      if (path.toFile.exists()) {
        try {
          val state = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
          val downloadsBackupInfo = state.parseJson.asJsObject.fields("downloads").asInstanceOf[JsArray].elements.toList.map(_.convertTo[DownloadBackupInfo])
          restoreState(downloadsBackupInfo)
          true
        } catch {
          case ex: Exception ⇒
            Main.controller.displayError(
              title = None,
              contentText = Some(s"${I18N.Strings.readIssue}\n$path"),
              ex = ex
            )
            false
        }
      } else {
        false
      }
    }

    readFile(Main.statePath) || readFile(PathsEx.backupPath(Main.statePath))

    janitor ! DownloadsJanitor.Start
  }

  private def canTryAcquireConnection: Boolean = this.synchronized {
    cnxTotal < Main.settings.cnxMax.get
  }

  def tryAcquireConnection(download: Download, force: Boolean): Option[AcquiredConnection] = this.synchronized {
    // Use the actual URI (since this is the real one we connect to)
    val uri = download.info.uri.get
    val siteSettings = Main.settings.getSite(uri)
    // Remember the acquired connection info to update the appropriate resources
    // in releaseConnection: the actual URI may change upon the first request
    // due to redirections.
    val acquired = AcquiredConnection(
      site = siteSettings.site,
      host = uri.getHost
    )

    val perSite = cnxPerSite(acquired.site)
    val perServer = cnxPerServer(acquired.host)

    val reasonOpt = {
      // The 'running downloads' limit applies to downloads that are not yet running
      if (force || download.isRunning) None
      else {
        val limit = Main.settings.downloadsMax.get
        val running = dlEntries.count(_.download.isRunning)
        if (running >= limit) Some(s"number of running downloads limit=<$limit>")
        else None
      }
    }.orElse {
      val limit = Main.settings.cnxMax.get
      if (!force && (cnxTotal >= limit)) Some(s"total number of connections limit=<$limit>")
      else None
    }.orElse {
      // 'site connections limit' only applies to identified site
      if (force || siteSettings.isDefault) None
      else {
        val limit = siteSettings.getCnxMax
        if (perSite >= limit) Some(s"number of connections for site=<${acquired.site}> limit=<$limit>")
        else None
      }
    }.orElse {
      val limit = Main.settings.cnxServerMax.get
      if (!force && (perServer >= limit)) Some(s"number of connections for host=<${acquired.host}> limit=<$limit>")
      else None
    }

    reasonOpt.foreach { reason ⇒
      if (download.activeSegments == 0) download.info.state.setValue(DownloadState.Pending)
      download.updateLastReason(Some(s"Limit reached: $reason"))
    }
    if (reasonOpt.isEmpty) {
      cnxTotal += 1
      cnxPerSite += (acquired.site → (perSite + 1))
      cnxPerServer += (acquired.host → (perServer + 1))
      download.openFile()
      download.info.state.setValue(DownloadState.Running)
      // Note: don't reset last reason. What really matters are 'new' reasons
      // that we could not acquire a connection.
      // Fact is that nominally we may otherwise log multiple times - and thus
      // with low usefulness - the same reason: when connection is released, we
      // usually can get a new one and on the next attempt reach the same limit
      // again.
      Some(acquired)
    } else {
      None
    }
  }

  def releaseConnection(acquired: AcquiredConnection): Unit = this.synchronized {
    val perSite = cnxPerSite(acquired.site)
    val perServer = cnxPerServer(acquired.host)

    if (cnxTotal > 0) cnxTotal -= 1
    if (perSite > 1) cnxPerSite += (acquired.site → (perSite - 1))
    else cnxPerSite -= acquired.site
    if (perServer > 1) cnxPerServer += (acquired.host → (perServer - 1))
    else cnxPerServer -= acquired.host
  }

}

case class AcquiredConnection(site: String, host: String)

object DownloadsJanitor {

  case class Janitor(clients: List[DownloadManager.LazyClient])
  case object Backup
  case object Cleanup
  case object Start
  case object Stop

}

class DownloadsJanitor(dlMngr: DownloadManager) extends Actor with StrictLogging {

  import DownloadsJanitor._

  // 'Old' (previously used) clients
  private var oldClients: Set[DownloadManager.LazyClient] = Set.empty
  // Current clients
  private var clients: List[DownloadManager.LazyClient] = Nil
  private var backupCancellable: Option[Cancelable] = None
  private var cleanupCancellable: Option[Cancelable] = None

  override def receive: Receive = {
    case Janitor(other) ⇒ janitor(other)
    case Backup ⇒ backup()
    case Cleanup ⇒ cleanup()
    case Start ⇒ start()
    case Stop ⇒ stop()
  }

  def janitor(other: List[DownloadManager.LazyClient]): Unit = {
    // Move current clients to 'old' ones
    oldClients ++= clients.filter(_.isStarted)
    clients = other
  }

  def start(): Unit = {
    // Prime the pump
    scheduleBackup()
    scheduleCleanup()
  }

  def backup(): Unit = {
    try {
      dlMngr.saveState()
    } catch {
      case ex: Exception ⇒
        Main.controller.displayError(
          title = None,
          contentText = Some(s"${I18N.Strings.writeIssue}\n${Main.statePath}"),
          ex = ex
        )
    }
    scheduleBackup()
    ()
  }

  def scheduleBackup(): Unit = {
    backupCancellable = Some(Main.scheduler.scheduleOnce(Main.settings.autosaveDelay.get)(self ! Backup))
  }

  def cleanup(connManager: PoolingNHttpClientConnectionManager): Unit = {
    // See: https://hc.apache.org/httpcomponents-client-ga/tutorial/html/connmgmt.html
    // When executing a new request on the client, pending requests past
    // timeout are failed and cached connections past keep-alive (assumed
    // infinite unless server says otherwise through dedicated header) are
    // pruned.
    // For a more 'active' janitoring, we have to trigger ourself the various
    // cleanup features.

    // Fail pending requests past timeout. There should be none since we
    // removed the max connection limits.
    connManager.validatePendingRequests()
    // Prune (cached) connections past keep-alive.
    connManager.closeExpiredConnections()
    val idleTimeout = Main.settings.idleTimeout.get
    // Prune (cached) idle connections.
    connManager.closeIdleConnections(idleTimeout.length, idleTimeout.unit)
  }

  def cleanup(): Unit = {
    // Cleanup all cnx managers
    oldClients.foreach { client ⇒
      val connManager = client.getConnectionManager
      cleanup(connManager)
      // Check if we are done with this old resource
      if (connManager.getTotalStats.getLeased == 0) {
        oldClients -= client
        // Properly close the client (shutdowns its manager) upon forgetting it
        client.close()
      }
    }
    clients.filter(_.isStarted).map(_.getConnectionManager).foreach(cleanup)
    scheduleCleanup()
  }

  def scheduleCleanup(): Unit = {
    // We want to check again later according to
    //  - connection request timeout
    //  - idle timeout
    //  - keep-alive: known per request, but have a hard limit of 5s here
    // For better handling check twice per minimum period,
    // and make sure to check at least every 5 seconds ...
    val next0 = List(
      Main.settings.connectionRequestTimeout.get / 2,
      Main.settings.idleTimeout.get / 2,
      5.seconds
    ).filter(_.length > 0).min
    // ... but not more than once per second.
    val next = List(
      next0,
      1.seconds
    ).max
    // Note: we may also simply schedule every second ...
    cleanupCancellable = Some(Main.scheduler.scheduleOnce(next)(self ! Cleanup))
  }

  def stop(): Unit = {
    backupCancellable.foreach(_.cancel())
    cleanupCancellable.foreach(_.cancel())
    // Close all remaining clients (shutdowns managers)
    (oldClients ++ clients).foreach(_.close())
    context.stop(self)
  }

}
