package suiryc.dl.mngr

import akka.actor.{Actor, ActorRef}
import com.typesafe.scalalogging.StrictLogging
import java.net.URI
import monix.execution.Cancelable
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.methods.{HttpGet, HttpHead, HttpRequestBase}
import org.apache.http.client.utils.URIUtils
import org.apache.http.entity.ContentType
import org.apache.http.nio.protocol.{AbstractAsyncResponseConsumer, BasicAsyncRequestProducer}
import org.apache.http.nio.{ContentDecoder, IOControl}
import org.apache.http.protocol.HttpContext
import org.apache.http._
import org.apache.http.client.protocol.HttpClientContext
import scala.collection.JavaConverters._
import scala.concurrent.Promise
import scala.concurrent.duration._
import suiryc.dl.mngr.model._
import suiryc.dl.mngr.util.Http


object FileDownloader {

  sealed trait FileDownloaderMsg
  case object DownloadStop extends FileDownloaderMsg
  case class DownloadResume(download: Download, restart: Boolean, force: Boolean) extends FileDownloaderMsg
  case class SegmentStarted(consumer: ResponseConsumer, response: HttpResponse) extends FileDownloaderMsg
  case class SegmentDone(consumer: ResponseConsumer, exOpt: Option[Exception]) extends FileDownloaderMsg
  case class Downloaded(range: SegmentRange) extends FileDownloaderMsg
  case object TrySegment extends FileDownloaderMsg
  case object Refresh extends FileDownloaderMsg
  case object AddConnection extends FileDownloaderMsg
  case object RemoveConnection extends FileDownloaderMsg
  case class TryConnection(promise: Promise[Unit]) extends FileDownloaderMsg

  case class SegmentHandlerData(
    range: SegmentRange,
    originalRange: SegmentRange,
    acquired: AcquiredConnection,
    forced: Boolean,
    context: HttpClientContext,
    request: HttpRequestBase,
    responseConsumer: ResponseConsumer,
    started: Boolean = false,
    aborted: Boolean = false
  ) {
    def abort(): SegmentHandlerData = {
      request.abort()
      copy(aborted = true)
    }
  }

  case class State(
    dlMngr: DownloadManager,
    download: Download,
    started: Boolean,
    stopping: Boolean = false,
    cnxErrors: Int = 0,
    dlErrors: Int = 0,
    failed: Option[DownloadException] = None,
    segmentConsumers: Map[ResponseConsumer, SegmentHandlerData] = Map.empty,
    trySegment: Option[Cancelable] = None,
    maxSegments: Option[Int] = None
  ) {

    updateMaxSegments()

    def getMaxSegments: Int = {
      // If the server does not accept ranges, maxSegments is automatically 1.
      if (download.info.acceptRanges.contains(false)) 1
      else maxSegments.getOrElse(download.maxSegments)
    }

    def setMaxSegments(max: Int): State = {
      copy(maxSegments = Some(max)).updateMaxSegments()
    }

    def updateMaxSegments(): State = {
      download.info.maxSegments.set(getMaxSegments)
      this
    }

    def withDownload(download: Download): State = copy(download = download)

    private def updatedConsumers(state: State): Unit = {
      download.info.activeSegments.setValue(state.segmentConsumers.count(_._2.started))
    }

    def addConsumer(consumer: ResponseConsumer, data: SegmentHandlerData): State = {
      val stateNew = copy(segmentConsumers = segmentConsumers + (consumer → data))
      updatedConsumers(stateNew)
      stateNew
    }

    def removeConsumer(consumer: ResponseConsumer): State = {
      val stateNew = copy(segmentConsumers = segmentConsumers - consumer)
      updatedConsumers(stateNew)
      stateNew
    }

    def updateConsumerData(consumer: ResponseConsumer)(f: SegmentHandlerData ⇒ SegmentHandlerData): State = {
      val stateNew = segmentConsumers.get(consumer).map { data ⇒
        copy(segmentConsumers = segmentConsumers + (consumer → f(data)))
      }.getOrElse(this)
      updatedConsumers(stateNew)
      stateNew
    }

    def cancelTrySegment: State = {
      trySegment.foreach(_.cancel())
      copy(trySegment = None)
    }

    def resume(restart: Boolean): State = {
      if (restart) {
        copy(
          started = false,
          stopping = false,
          cnxErrors = 0,
          dlErrors = 0,
          failed = None,
          segmentConsumers = Map.empty,
          maxSegments = None
        )
      } else {
        copy(
          stopping = false,
          cnxErrors = 0,
          dlErrors = 0,
          failed = None,
          maxSegments = None
        )
      }
    }

  }

}

class FileDownloader(dlMngr: DownloadManager, dl: Download) extends Actor with StrictLogging {

  import FileDownloader._

  // See RFC 7233 for range requests

  // TODO: recheck range validator/size etc if 'started' is true initially ?
  override def receive: Receive = receive(State(dlMngr, dl, started = dl.info.isSizeDetermined))

  def receive(state: State): Receive = {
    case DownloadResume(download, restart, force) ⇒
      val action = if (restart) {
        "re-starting"
      } else if (state.started) {
        "resuming"
      } else {
        "starting"
      }
      applyState(resume(state.resume(restart).withDownload(download), action, force))

    case DownloadStop ⇒
      applyState(stop(state, DownloadException(message = "Download stopped", stopped = true), abort = true))

    case TryConnection(promise) ⇒
      applyState(tryConnection(state, promise))

    case AddConnection ⇒
      applyState(trySegment(state, force = true))

    case RemoveConnection ⇒
      applyState(stopSegment(state))

    case TrySegment ⇒
      applyState(trySegment(state.cancelTrySegment))

    case SegmentStarted(consumer, response) ⇒
      applyState(segmentStarted(state, consumer, response))

    case SegmentDone(consumer, exOpt) ⇒
      applyState(segmentDone(state, consumer, exOpt))

    case Downloaded(range) ⇒
      downloaded(state, range)

    case Refresh ⇒
      refresh(state)
  }

  def applyState(state: State): Unit = {
    context.become(receive(state))
  }

  def refresh(state: State): Unit = {
    state.updateMaxSegments()
    ()
  }

  def resume(state: State, action: String, force: Boolean): State = {
    val message = s"Download $action"
    logger.info(s"${state.download.context} $message")
    state.download.info.addLog(LogKind.Info, message)
    trySegment(state, force = force)
  }

  def stop(state0: State, ex: DownloadException, abort: Boolean): State = {
    val download = state0.download
    logger.info(s"${download.context} Download stopping")
    download.info.addLog(LogKind.Info, "Download stopping")

    val state = state0.copy(
      stopping = true,
      failed = Some(ex)
    )
    if (state.segmentConsumers.isEmpty) {
      done(state)
    } else if (abort) {
      // Stop all segments
      state.segmentConsumers.keys.foldLeft(state) { (state, consumer) ⇒
        state.updateConsumerData(consumer)(_.abort())
      }
    } else {
      // Let segments finish (only prevent new segments)
      state
    }
  }

  def downloaded(state: State, range: SegmentRange): Unit = {
    state.download.info.remainingRanges match {
      case Some(remainingRanges) ⇒
        remainingRanges.remove(range)
        state.download.info.downloaded.setValue(remainingRanges.getRemovedLength)
        logger.debug(s"${state.download.context} Downloaded range=<$range>; remaining $remainingRanges")

      case None ⇒
        state.download.info.downloaded.setValue(range.end + 1)
    }
  }

  def changeSegmentStart(state: State, consumer: ResponseConsumer, start: Long): State = {
    state.updateConsumerData(consumer) { data ⇒
      data.copy(range = data.range.copy(start = start))
    }
  }

  def changeSegmentEnd(state: State, consumer: ResponseConsumer, end: Long): State = {
    val data = state.segmentConsumers(consumer)
    val range0 = data.range
    val range = range0.copy(end = end)
    logger.info(s"${state.download.context(range0)} Change active segment range end from=<${range0.end}> to=<${range.end}>")
    data.responseConsumer.end = end
    state.updateConsumerData(consumer) { data ⇒
      data.copy(
        range = range,
        // Update original range if we got the actual end
        originalRange = if (range0.end < 0) range else data.originalRange
      )
    }
  }

  def tryConnection(state: State, promise: Promise[Unit]): State = {
    // Pending segment trying counts as connection trying.
    try {
      if (state.trySegment.isEmpty) {
        trySegment(state)
      } else state
    } finally {
      promise.trySuccess(())
      ()
    }
  }

  def trySegment(state0: State, force: Boolean = false): State = {
    // Notes:
    // Active segments should complete all remaining segments. Some active
    // segments may not have yet started to download.
    // The best candidate is the active segment with the largest remaining
    // range that we will split in two.
    // We can only start a new segment if ranges are accepted and we did not yet
    // reached max number of segments (and a TrySegment is not currently
    // scheduled).
    val download = state0.download
    lazy val tryAcquireConnection = state0.dlMngr.tryAcquireConnection(download, force)
    lazy val canAddSegment = {
      val limit = state0.getMaxSegments
      val ok = state0.segmentConsumers.size < limit
      if (!ok) download.updateLastReason(Some(s"Limit reached: number of segments limit=<$limit>"))
      ok
    }
    lazy val canStartSegment = !state0.stopping && state0.trySegment.isEmpty && (force || canAddSegment)
    if (download.info.acceptRanges.contains(true) && canStartSegment) {
      // Clone the current remaining ranges (we will work with it)
      download.info.remainingRanges.map(_.clone()).flatMap { remainingRanges ⇒
        val minSize = download.minSegmentSize
        val remaining = remainingRanges.getRanges
        // Get all active remaining ranges
        val remainingActive = state0.segmentConsumers.toList.flatMap { case (consumer, data) ⇒
          val activeRange = data.range
          remaining.asScala.find { remainingRange ⇒
            // Find the remaining range that this active segment is working on
            ((remainingRange.start >= activeRange.start) && (remainingRange.start < activeRange.end)) ||
              ((remainingRange.start < activeRange.start) && (remainingRange.end > activeRange.start))
          }.map { remainingRange ⇒
            // Only get the remaining range for this active segment
            val range = remainingRange.copy(
              start = math.max(remainingRange.start, activeRange.start),
              end = math.min(remainingRange.end, activeRange.end)
            )
            // Note: if an active range is to be picked it will be split in two.
            (Some(consumer), range, range.length / 2)
          }.toList.filter(_._3 >= minSize)
        }

        // Get all remaining non-active ranges
        state0.segmentConsumers.values.foreach { data ⇒
          remainingRanges.remove(data.range)
        }
        // Note: non-active segments are not subject to size limit
        val remainingNonActive = remainingRanges.getRanges.asScala.toList.map { range ⇒
          (Option.empty[ResponseConsumer], range, range.length)
        }

        // Get the best candidate
        (remainingActive ::: remainingNonActive).sortBy(-_._3).headOption.flatMap {
          case (consumerOpt, range, _) ⇒
            // Now that we know we really can start a new segment, ensure we can
            // actually create a new connection.
            tryAcquireConnection.map { acquired ⇒
              (consumerOpt, range, acquired)
            }
        }
      }.map {
        case (consumerOpt, range, acquired) ⇒
          consumerOpt match {
            case Some(consumer) ⇒
              // TODO: wait for request 'success' before changing current consumer end ?
              //  -> useful if request actually fails, so that we
              //    1. don't waste time/logs changing and resetting the current consumer end
              //    2. prevent current consumer to end if it reaches the end before the request
              //       actually fails
              //  -> at worst, new consumer may overwrite sections already downloaded ?
              //  Is is worth it ?
              val newRange = range.copy(
                start = range.start + range.length / 2 + 1
              )
              val state1 = changeSegmentEnd(state0, consumer, newRange.start - 1)
              segmentStart(state1, newRange, acquired, forced = force)

            case None ⇒
              segmentStart(state0, range, acquired, forced = force)
          }
      }.getOrElse(state0)
    } else if (download.info.remainingRanges.isEmpty && state0.segmentConsumers.isEmpty && !state0.stopping) {
      // Special case: initial request failed, so re-try it (if possible)
      tryAcquireConnection.map { acquired ⇒
        segmentStart(state0, SegmentRange(0, -1), acquired, forced = force)
      }.getOrElse(state0)
    } else {
      // We cannot try a new segment.
      // We also end up here if first connection request is ongoing (no response
      // yet); may happen if a TryConnection is triggered.
      state0
    }
  }

  def segmentStart(state: State, range: SegmentRange, acquired: AcquiredConnection, forced: Boolean): State = {
    val download = state.download
    val uri = download.info.uri.get
    download.rateLimiter.addDownload()
    try {
      logger.info(s"${download.context(range)} Starting range=$range")
      state.download.info.addLog(LogKind.Debug, "Starting new segment")

      // Upon 'resuming' (!started and downloaded already set), only do a HEAD
      // request to determine size, accept ranges, etc. The way requests are
      // handled the response consumer will end upon receiving the response, on
      // which we updated our state (actual remaining ranges etc), and a new
      // segment will be tried right away (starting from the already downloaded
      // offset).
      val downloaded = download.info.downloaded.get
      val request = if (!state.started && (downloaded > 0)) {
        new HttpHead(uri)
      } else {
        new HttpGet(uri)
      }

      download.referrer.foreach { referrer ⇒
        request.addHeader(HttpHeaders.REFERER, referrer.toASCIIString)
      }
      download.cookie.foreach { cookie ⇒
        // Standard way would be to parse the cookie, create a cookie store and
        // attach it to the client or at least the request context. On the other
        // hand, it is easier and faster to set the header as requested.
        request.addHeader("Cookie", cookie)
      }
      download.userAgent.foreach { userAgent ⇒
        request.addHeader(HttpHeaders.USER_AGENT, userAgent)
      }
      if (range.length > 0) {
        request.addHeader(HttpHeaders.RANGE, s"bytes=${range.start}-${range.end}")
        download.info.rangeValidator.foreach(v ⇒ request.addHeader(HttpHeaders.IF_RANGE, v))
      }
      val rcb = RequestConfig.custom
        .setConnectionRequestTimeout(Main.settings.connectionRequestTimeout.get.toMillis.toInt)
        .setConnectTimeout(Main.settings.connectTimeout.get.toMillis.toInt)
        .setSocketTimeout(Main.settings.socketTimeout.get.toMillis.toInt)
      Main.settings.proxy.opt.map(_.trim).filterNot(_.isEmpty).foreach { proxy0 ⇒
        try {
          // Cleanup URI
          val proxyUri = URI.create(proxy0)
          val proxy = s"${proxyUri.getScheme}://${proxyUri.getAuthority}"
          rcb.setProxy(HttpHost.create(proxy))
        } catch {
          case ex: Exception ⇒
            val message = s"Cannot set proxy: ${ex.getMessage}"
            logger.error(s"${download.context(range)} $message", ex)
            state.download.info.addLog(LogKind.Error, message, Some(ex))
        }
      }
      request.setConfig(rcb.build)
      val requestProducer = new BasicAsyncRequestProducer(URIUtils.extractHost(uri), request)

      // Attach a context so that we can retrieve redirection URIs.
      // It happens that redirection URIs are stored in the HTTP context.
      // An alternative would be to add an interceptor:
      //   - in the shared client, and use the context, possibly to execute a
      //     specific callback
      //  or
      //   - in a one-shot client, with a specific callback to execute
      val context = HttpClientContext.create()
      val responseConsumer = new ResponseConsumer(self, download, range, request)
      state.dlMngr.client.execute(requestProducer, responseConsumer, context, null)

      val data = SegmentHandlerData(
        range = range,
        originalRange = range,
        acquired = acquired,
        forced = forced,
        context = context,
        request = request,
        responseConsumer = responseConsumer
      )
      state.addConsumer(responseConsumer, data)
    } catch {
      case ex: Exception ⇒
        val message = s"Failed to start segment range=$range download: ${ex.getMessage}"
        logger.error(s"${download.context(range)} $message", ex)
        state.download.info.addLog(LogKind.Error, message, Some(ex))
        segmentDone(state, None, range, acquired, Some(ex))
    }
  }

  def segmentStarted(state0: State, consumer: ResponseConsumer, response: HttpResponse): State = {
    val download = state0.download
    val state1 = if (state0.started) {
      // We don't need anything anymore, this serves only to trigger a new
      // segment start if possible.
      state0.copy(cnxErrors = 0)
    } else {
      val contentLength = Http.getContentLength(response)
      val lastModified = Http.getLastModified(response)
      val acceptRanges = Http.handleBytesRange(response, contentLength)
      val validator = if (acceptRanges) Http.getValidator(response) else None

      // Update actual URI when applicable, so that next requests will use it
      // directly.
      state0.segmentConsumers.get(consumer).flatMap { data ⇒
        Option(data.context.getRedirectLocations)
      }.foreach { redirectLocations ⇒
        if (!redirectLocations.isEmpty) {
          download.info.uri.set(redirectLocations.asScala.last)
          val message = s"Actual (redirected) uri=<${download.info.uri.get}>"
          logger.info(s"${download.context} $message")
          download.info.addLog(LogKind.Info, message)
        }
      }
      val message = s"Download contentLength=<$contentLength>${
        lastModified.map(v ⇒ s" lastModified=<$v>").getOrElse("")
      } acceptRanges=<$acceptRanges>${
        validator.map(v ⇒ s" validator=<$v>").getOrElse("")
      }"
      logger.info(s"${download.context} $message")
      download.info.addLog(LogKind.Info, message)

      if (contentLength < 0) download.info.addLog(LogKind.Warning, "Download size is unknown")

      download.info.size.set(contentLength)
      download.info.remainingRanges = if (contentLength >= 0) Some(new SegmentRanges(contentLength)) else None
      download.info.rangeValidator = validator
      download.info.acceptRanges = Some(acceptRanges)
      download.info.lastModified = lastModified

      if (!acceptRanges) {
        download.info.addLog(LogKind.Warning, "Download resuming is not supported")
        // maxSegments will automatically be 1 now
        state0.updateMaxSegments()
      }

      // If 'downloaded' is already set, we are resuming an existing file
      // assuming its current size was already downloaded.
      val downloaded = download.info.downloaded.get
      if (downloaded > 0) {
        download.info.remainingRanges.foreach { remainingRanges ⇒
          remainingRanges.remove(SegmentRange(0, downloaded - 1))
        }
      }
      val state1 = if (contentLength >= 0) changeSegmentEnd(state0, consumer, contentLength - 1) else state0
      state1.copy(
        started = true,
        cnxErrors = 0
      )
    }
    val state2 = state1.updateConsumerData(consumer)(_.copy(started = true))
    // If the segment/connection was forced, and we went beyond the segment
    // count limit, keep this new limit in mind.
    // Only try a new segment when applicable.
    val active = download.info.activeSegments.get
    if (state2.segmentConsumers(consumer).forced && (active > state2.getMaxSegments)) {
      state2.setMaxSegments(active)
    } else {
      trySegment(state2)
    }
  }

  def segmentDone(state: State, consumer: ResponseConsumer, ex: Option[Exception]): State = {
    state.segmentConsumers.get(consumer).map { data ⇒
      segmentDone(state.removeConsumer(consumer), Some(data), data.range, data.acquired, ex)
    }.getOrElse(state)
  }

  def segmentDone(state0: State, dataOpt: Option[SegmentHandlerData], range: SegmentRange, acquired: AcquiredConnection, ex0: Option[Exception]): State = {
    val download = state0.download
    state0.dlMngr.releaseConnection(acquired)
    download.rateLimiter.removeDownload()
    // Take into account our failure if any.
    val exOpt0: Option[DownloadException] = ex0.map {
      case ex0: DownloadException ⇒ ex0
      case ex0: Exception ⇒ DownloadException(message = ex0.getMessage, cause = ex0, started = dataOpt.exists(_.started))
    }
    // Note: aborting request should not have triggered an exception.
    val aborted = dataOpt.exists(_.aborted)
    val exOpt: Option[DownloadException] = if (!aborted) {
      exOpt0
    } else {
      None
    }

    val state1 = handleWriteError(state0, exOpt0)
    val state2 = exOpt match {
      case Some(ex) ⇒
        state1.copy(
          cnxErrors = state1.cnxErrors + (if (!ex.started) 1 else 0),
          dlErrors = state1.dlErrors + (if (ex.started) 1 else 0)
        )

      case None ⇒
        state1
    }
    val status = if (aborted) {
      "aborted"
    } else if (exOpt.isDefined) {
      "failed"
    } else {
      "finished"
    }
    val message = s"${download.context(range)} Segment $status${
      exOpt.map(v ⇒ s" ex=<$v>").getOrElse("")
    }; remaining segments=${download.info.activeSegments.getValue}"
    exOpt match {
      case Some(ex) ⇒
        logger.error(message, ex)
        state2.download.info.addLog(LogKind.Error, s"Segment $status: ${ex.getMessage}", Some(ex))

      case None ⇒
        logger.info(message)
        state2.download.info.addLog(LogKind.Debug, s"Segment $status")
    }

    // Upon issue, and if range was not started, try to give it back to its
    // original owner.
    val state3 = if (exOpt.isDefined && download.info.remainingRanges.exists(_.contains(range.start))) {
      state2.segmentConsumers.find {
        case (_, handerData) ⇒
          // The range to give back must follow an handler end, and belong to
          // its original range.
          (range.start == handerData.range.end + 1) && (range.end <= handerData.originalRange.end)
      }.map {
        case (consumer, _) ⇒
          changeSegmentEnd(state2, consumer, range.end)
      }.getOrElse(state2)
    } else {
      state2
    }

    // Note:
    // Download is not fully finished if segments are still running or
    // remaining. If content length was unknown, we are done if the segment
    // finished without issue.
    val finished = state3.segmentConsumers.isEmpty && (
      state3.stopping ||
        (download.info.remainingRanges.isEmpty && exOpt.isEmpty) ||
        download.info.remainingRanges.exists(_.getRanges.isEmpty)
      )
    val state = if (finished) {
      done(state3)
    } else if ((state3.cnxErrors < Main.settings.errorMax.get) && (state3.dlErrors < Main.settings.errorMax.get)) {
      segmentFinished(state3, aborted, exOpt)
    } else {
      tooManyErrors(state3, exOpt)
    }

    // Now that we may have tried a new connection, give a chance to other
    // downloads to also try a new connection (if applicable).
    // If we were done, let the download manager (which asynchronously handles
    // the 'final' state) do it (especially after changing the state).
    if (!finished) state.dlMngr.tryConnection(Some(download.id))

    state
  }

  def handleWriteError(state: State, exOpt: Option[DownloadException]): State = {
    val ranges = exOpt.map(_.rangesWriteFailed).getOrElse(Nil)

    if (ranges.nonEmpty) {
      // Re-add failed ranges to remaining ones.
      state.download.info.remainingRanges.foreach { remainingRanges ⇒
        ranges.foreach(remainingRanges.add)
        state.download.info.downloaded.setValue(remainingRanges.getRemovedLength)
      }
      // Update active segments start so that failed ranges are not seen as
      // 'active' and can be more easily picked in trySegment.
      ranges.foldLeft(state) { (state, range) ⇒
        // 1. Loop over failed ranges
        state.segmentConsumers.values.filter { data ⇒
          // 2. Find active ranges that overlap with the failed range
          // Note: the 'worst case' is when consecutive handlers barely finished
          // (end of downloaded range sent, but 'SegmentDone' not yet received)
          // and another segment triggered the write failure for all those
          // ranges. In this case, more than one consumer are concerned by
          // the (merged) failed range.
          (data.range.start <= range.end) && (data.range.end >= range.start)
        }.foldLeft(state) { (state, data) ⇒
          // 3. Update the active range start
          changeSegmentStart(state, data.responseConsumer, math.min(data.range.end, range.end) + 1)
        }
      }
    } else {
      state
    }
  }

  def segmentFinished(state: State, aborted: Boolean, exOpt: Option[DownloadException]): State = {
    // We may try to start a new segment
    if (exOpt.isEmpty) {
      // Don't try a new segment if this one was aborted, unless we are below
      // the limit (should be because limit is 1 and we aborted the last
      // active segment).
      if (!aborted || (state.segmentConsumers.size < state.getMaxSegments)) {
        trySegment(state)
      } else {
        state
      }
    } else {
      // Delay next attempt upon issue (unless already ongoing)
      if (state.trySegment.isEmpty) {
        state.copy(
          trySegment = Some(Main.scheduler.scheduleOnce(Main.settings.errorDelay.get)(self ! TrySegment))
        )
      } else {
        state
      }
    }
  }

  def tooManyErrors(state0: State, exOpt: Option[DownloadException]): State = {
    // Too many issues.
    val download = state0.download
    exOpt.map { ex ⇒
      if (ex.rangeFailed && (ex.rangeValidator != download.info.rangeValidator)) {
        // The range request failed apparently because the validator changed
        // (file on server was changed since we started ?).
        // Notify caller through promise, and let it decide whether to end the
        // download or restart it (from its current state).
        val message = s"Too many errors; range validator changed from=<${download.info.rangeValidator}> to=<${ex.rangeValidator}>"
        logger.warn(s"${download.context} $message")
        state0.download.info.addLog(LogKind.Warning, message)
        stop(state0, ex, abort = true)
      } else {
        val state1 = if (!ex.started) {
          // Request failed.
          // Assume we cannot create more segments than what we currently have.
          // (but keep a minimum of 1)
          val maxSegments = math.max(1, state0.segmentConsumers.size)
          val message = s"Too many errors; set maxSegments=<$maxSegments>"
          logger.warn(s"${download.context} $message")
          state0.download.info.addLog(LogKind.Warning, message)
          state0.setMaxSegments(maxSegments)
        } else {
          // Segment had an issue after successfully starting.
          val message = "Too many errors"
          logger.warn(s"${download.context} $message")
          state0.download.info.addLog(LogKind.Warning, message)
          state0
        }
        stop(state1, ex, abort = false)
      }
    }.getOrElse(state0)
    // else: no Exception (this segment finished without issue)
  }

  def stopSegment(state0: State): State = {
    val segments = state0.segmentConsumers.size

    // Note: the number of segments may be beyond the limit (should be 1 beyond
    // for a forced segment that has not 'started' yet).
    val state = if (segments >= state0.getMaxSegments) {
      // Select the farthest segment, and abort it
      val data = state0.segmentConsumers.values.toList.maxBy(_.range.start)
      state0.updateConsumerData(data.responseConsumer)(_.abort())
    } else {
      state0
    }
    // Reduce the segments limit (relatively to the current number of segments)
    state.setMaxSegments(math.max(1, segments - 1))
  }

  def done(state0: State): State = {
    val download = state0.download
    val lastModified = state0.download.info.lastModified
    val complete = (download.info.remainingRanges.isEmpty && state0.started && state0.failed.isEmpty) ||
      download.info.remainingRanges.exists(_.getRanges.isEmpty)

    // If we had a failure, but download actually completed (e.g. only one
    // segment could be started, and the whole download range was completed)
    // ignore the failure.
    val state1 = if (complete && state0.failed.nonEmpty) {
      state0.copy(failed = None)
    } else {
      state0
    }

    // Close the file and handle any issue.
    val closeExOpt = try {
      state1.download.closeFile(lastModified, done = state1.failed.isEmpty)
      None
    } catch {
      case ex: DownloadException ⇒
        Some(ex)

      case ex: Exception ⇒
        Some(DownloadException(
          message = s"I/O error: (${ex.getClass.getSimpleName}) ${ex.getMessage}",
          cause = ex,
          started = state1.started
        ))
    }

    val state = handleWriteError(state1, closeExOpt)
    state.failed.orElse(closeExOpt) match {
      case Some(ex) if closeExOpt.isEmpty ⇒
        // The download did fail on its own
        state.download.promise.tryFailure(ex)
        // Do not stop the actor (we can still resume/restart)
        state

      case Some(ex) ⇒
        // The download was actually ok, but closing failed
        logger.error(s"${download.context} Download uri=<${download.uri}> error=<${ex.getMessage}>", ex)
        download.info.addLog(LogKind.Error, s"Download error: ${ex.getMessage}", Some(ex))
        // Re-try right now
        segmentFinished(state, aborted = false, None)

      case None ⇒
        // We are really done (success)
        logger.info(s"${download.context} Download uri=<${download.uri}> done: success${
          lastModified.map(v ⇒ s" (file date=<$v>)").getOrElse("")
        }")
        state.download.promise.trySuccess(())
        // Time to stop ourself
        context.stop(self)
        state
    }
  }

}

class ResponseConsumer(
  downloadHandler: ActorRef,
  download: Download,
  range: SegmentRange,
  request: HttpRequestBase
) extends AbstractAsyncResponseConsumer[Unit] {

  private var position: Long = range.start
  @volatile private[mngr] var end: Long = range.end

  // IOControl will be accessed by concurrent threads.
  // For our usage, a simple 'synchronized' is enough.
  private var ioCtrl = Option.empty[IOControl]

  private def suspend(ioctrl: IOControl): Unit = this.synchronized {
    ioctrl.suspendInput()
    ioCtrl = Some(ioctrl)
  }

  private def resume(): Unit = this.synchronized {
    ioCtrl.foreach(_.requestInput())
    ioCtrl = None
  }

  override def onResponseReceived(response: HttpResponse): Unit = {
    val statusLine = response.getStatusLine
    val failure = if (statusLine.getStatusCode / 100 != 2) {
      // Request failed with HTTP code other than 2xx
      Some(DownloadException(s"Request failed with HTTP status=<(${statusLine.getStatusCode}) ${statusLine.getReasonPhrase}>"))
    } else if ((position > 0) || (end >= 0)) {
      // We requested a partial content; ensure the response is consistent.
      // We could check the requested range is returned, or the validator is
      // the one we used. But that's the server responsibility to only return
      // the appropriate HTTP code when all is fine.
      if (response.getStatusLine.getStatusCode != HttpStatus.SC_PARTIAL_CONTENT) {
        Some(DownloadException(
          message = s"Range request not honored; HTTP status=<(${statusLine.getStatusCode}) ${statusLine.getReasonPhrase}>",
          rangeFailed = true,
          rangeValidator = Http.getValidator(response)
        ))
      } else {
        None
      }
    } else {
      None
    }

    // Note: 'failed' will fail this request, which will release its resources
    // and propagate the issue to handler.
    failure match {
      case Some(ex) ⇒
        // First set failure
        failed(ex)
        // Then abort the request.
        // It is sometimes necessary: for some URIs, 'failed' is enough to stop
        // the request, for others the connection actually keeps on receiving
        // data (and stays alive in the background ...)
        request.abort()

      case None ⇒
        downloadHandler ! FileDownloader.SegmentStarted(this, response)
    }
  }

  override def onContentReceived(decoder: ContentDecoder, ioctrl: IOControl): Unit = {
    // Notes:
    // Decoder may require more than one read to empty the received content.
    // Suspending input takes immediate effect: we are not called until we
    // resume.
    //
    // If we don't consume all the data available in the decoder, we will be
    // called again with what remains (and possibly some more received since
    // then).
    // So we can apply rate limiting by lowering the buffer size and only
    // read once per call. If limit is reached, we suspend input until next
    // time we can consume again.
    @scala.annotation.tailrec
    def loop(): Boolean = {
      // Use hint when rate is limited
      val count0 = if (end >= 0) end + 1 - position else Long.MaxValue
      val count = math.min(count0, download.rateLimiter.getReadSizeHint)
      // Zero copy: transfer received content directly to file.
      // This require to do it right now: if we delegate, say to the parent
      // actor, we will be called again (decoder not empty) at least until
      // decoder is processed (assuming the decoder is thread-safe).
      val transferred = try {
        download.downloadFile.write(decoder, position, count, downloadHandler)
      } catch {
        case ex: DownloadException ⇒
          failed(ex)
          0L

        case ex: Exception ⇒
          failed(DownloadException(
            message = s"I/O error: (${ex.getClass.getSimpleName}) ${ex.getMessage}",
            cause = ex,
            started = true
          ))
          0L
      }
      if (transferred > 0) {
        position += transferred
        download.rateLimiter.consumeTokens(transferred)

        // Check whether we are done with this
        val endReached = (end >= 0) && (position > end)
        // Do not keep on consuming the decoder if rate is limited.
        // If data remain, we will be called again, which let some more chances
        // for other downloads to consume too.
        // Leave loop if transferred size is lower than requested; usually means
        // there is nothing more buffered (at worst we will be called again).
        // TODO: the very first call exceeds 'count' ...
        if (!endReached && !download.rateLimiter.isLimited && (transferred >= count)) loop()
        else endReached
      } else {
        false
      }
    }

    val skip = if (download.rateLimiter.getAvailableTokens <= 0) {
      // Limit reached, suspend download until new tokens are available
      val delay = download.rateLimiter.nextSliceDelay
      suspend(ioctrl)
      Main.scheduler.scheduleOnce(delay.millis) {
        resume()
      }
      true
    } else {
      false
    }

    if (!skip && !isDone) {
      val endReached = loop()
      if (endReached) ioctrl.shutdown()
    }
  }

  override def onEntityEnclosed(entity: HttpEntity, contentType: ContentType): Unit = {
    // We don't care; we only process content through onContentReceived
  }

  override def buildResult(context: HttpContext): Unit = {
    // We don't have a result
  }

  override def releaseResources(): Unit = {
    // Note: if we reduced the download end (new segments created), the channel
    // shutdowns triggers an "Connection closed unexpectedly" error. But we
    // don't care about errors as long as we reached the segment end.
    Option(getException) match {
      case Some(ex) if (end < 0) || (position < end) ⇒
        downloadHandler ! FileDownloader.SegmentDone(this, Some(ex))

      case _ ⇒
        downloadHandler ! FileDownloader.SegmentDone(this, None)
    }
  }

}
