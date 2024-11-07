package suiryc.dl.mngr

import akka.actor.{Actor, ActorRef}
import com.typesafe.scalalogging.StrictLogging
import monix.execution.Cancelable
import org.apache.http.client.methods.HttpRequestBase
import org.apache.http.client.utils.URIUtils
import org.apache.http.entity.ContentType
import org.apache.http.nio.protocol.{AbstractAsyncResponseConsumer, BasicAsyncRequestProducer}
import org.apache.http.nio.{ContentDecoder, IOControl}
import org.apache.http.protocol.HttpContext
import org.apache.http._
import org.apache.http.client.protocol.HttpClientContext
import org.apache.http.nio.conn.ManagedNHttpClientConnection
import suiryc.dl.mngr.model._
import suiryc.dl.mngr.util.Http
import suiryc.scala.concurrent.RichFuture
import suiryc.scala.misc.Units

import java.nio.file.{Files, NoSuchFileException}
import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success}

// Notes:
// DownloadManager globally manages all downloads.
// It takes care of each download state change:
//  - requested from user: e.g. adding new download, starting/resuming, trying
//    starting a new segment, etc.
//  - when download ends (following the associated Promise) successfully or
//    with error
//  - etc.
// It takes care of connections life-cycle, in order to enforce limits when
// applicable.
//
// FileDownloader manages one download.
// It takes care of requested action to start/resume/stop, etc.
// It handles the download in segments. There is at least one segment for the
// whole content, but if the server handle ranges it can be divided in more
// segments (each one downloading a range of the content), depending on limits:
// number of segments per download, number of connections allowed (per
// server/site) etc.
//
// DownloadFile manages the actual underlying file.
// It takes care of creation/renaming when applicable: temporary file, name
// already taken when needed, etc.
// It takes care of I/O, especially concurrent accesses when writing, along with
// errors (invalidating parts of downloaded ranges) upon flushing data on disk:
// upon write error, an Exception listing invalidated ranges is thrown and
// FileDownloader takes care of re-downloading them.

object FileDownloader {

  sealed trait FileDownloaderMsg
  // Request to stop download.
  case object DownloadStop extends FileDownloaderMsg
  // Request to start/restart/resume download: only changes internal state.
  // New connection attempt is managed by DownloadManager.
  case class DownloadResume(restart: Boolean) extends FileDownloaderMsg
  // Internal: segment (request) successfully started.
  case class SegmentStarted(consumer: ResponseConsumer, response: HttpResponse) extends FileDownloaderMsg
  // Internal: segment ended, successfully or not.
  case class SegmentDone(consumer: ResponseConsumer, error: Option[Exception]) extends FileDownloaderMsg
  // Internal: a byte range was downloaded (written in file, but not necessarily
  // flushed on disk).
  // Note that this comes from DownloadFile (see comments there) and not
  // FileDownloader itself.
  case class Downloaded(range: SegmentRange) extends FileDownloaderMsg
  // Internal: try to start a new segment; triggered after a previous one ended
  // with error, waiting the configured delay before next attempt.
  private case object TrySegment extends FileDownloaderMsg
  // Refresh download after settings changes.
  case object Refresh extends FileDownloaderMsg
  // Request to (force) add a new connection: starts a new segment.
  case object AddConnection extends FileDownloaderMsg
  // Removes one active connection: stops one segment (maybe the download
  // itself if there is only one active segment).
  case object RemoveConnection extends FileDownloaderMsg
  // Tries to start a new segment or an existing one.
  case class TryConnection(promise: Promise[Unit]) extends FileDownloaderMsg
  // Internal: resume download or re-try (new segment); triggered when an SSL
  // error was encountered and we now 'trustAll' for the server/site.
  private case object TryResume

  protected case class TryCnxData(caller: Promise[Unit], attempt: Promise[Unit] = Promise()) {

    import RichFuture._
    import Main.Akka._

    // When caller wants us to try a cnx, we try a new segment if possible.
    // If attempt fails, upon timeout, or we cannot try a new segment, complete
    // the caller promise. But if the attempt succeeds, and we can try another
    // segment, renew the attempt (until we cannot anymore, fail or timeout).

    attempt.future.withTimeout(Main.settings.errorDelay.get).onComplete {
      case Failure(_) => done()
      case _ =>
    }

    def renew: TryCnxData = {
      attemptSuccess()
      copy(attempt = Promise())
    }

    def completeWith(other: TryCnxData): Unit = {
      attemptSuccess()
      caller.completeWith(other.caller.future)
    }

    def attemptFailure(ex: Exception): Unit = {
      attempt.tryFailure(ex)
      ()
    }

    private def attemptSuccess(): Unit = {
      attempt.trySuccess(())
      ()
    }

    def done(): Unit = {
      caller.trySuccess(())
      ()
    }

  }

  case class SegmentHandlerData(
    range: SegmentRange,
    originalRange: SegmentRange,
    acquired: AcquiredConnection,
    forced: Boolean,
    tryCnx: Option[TryCnxData],
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
    tryCnx: Option[TryCnxData] = None,
    trySegment: Option[Cancelable] = None,
    maxSegments: Option[Int] = None,
    forceable: Int = 0
  ) {

    updateMaxSegments()

    def isActive: Boolean = trySegment.nonEmpty || segmentConsumers.nonEmpty

    def isComplete: Boolean = {
      // We are complete if either:
      //  - ranges are supported, and there is no remaining one
      //  - ranges are not supported, download started, is not active anymore
      //    and there was no error (includes user stopping it)
      (download.info.remainingRanges.isEmpty && started && failed.isEmpty && !isActive) ||
        download.info.remainingRanges.exists(_.getRanges.isEmpty)
    }

    def addError(ex: DownloadException, downloaded: Boolean): State = {
      copy(
        cnxErrors = cnxErrors + (if (!ex.started) 1 else 0),
        dlErrors = dlErrors + (if (ex.started && !downloaded) 1 else 0)
      )
    }

    def hasTooManyErrors: Boolean = {
      (cnxErrors >= Main.settings.errorMax.get) || (dlErrors >= Main.settings.errorMax.get)
    }

    def resetCnxErrors: State = copy(cnxErrors = 0)

    def resetErrors: State = resetCnxErrors.copy(dlErrors = 0)

    def getSegments: Int = download.info.segments.get

    def getMaxSegments: Int = {
      // If the server does not accept ranges, or if size is unknown,
      // maxSegments is automatically 1.
      if (download.acceptRanges.contains(false) || download.info.isSizeUnknown) 1
      else maxSegments.getOrElse(download.maxSegments)
    }

    def setMaxSegments(max: Int): State = copy(maxSegments = Some(max)).updateMaxSegments()

    def updateMaxSegments(): State = {
      download.info.maxSegments.set(getMaxSegments)
      this
    }

    // Notes on forced connections:
    // A new connection can be manually forced. When it happens, we remember
    // it so that for this download we may force again a new connection when
    // a current one ends.
    // When a forced connection ends, we increment a 'forceable' counter (unless
    // the connection was manually aborted, or there were too many errors).
    // When we check whether a new connection can start, we force one if the
    // 'forceable' counter is positive (and we decrement it).
    // If we have too many errors, the counter is resetted to 0 to prevent
    // automatically forcing new connections.
    //
    // If the number of segments exceeds the nominal (site-related) limit, we
    // remember the new limit. If we manually remove a connection, we also
    // reduce our limit (relatively to the number of running segments).
    //
    // This ensures a minimal handling of forced connections to go beyond the
    // connections/segments limits, whether to force starting a download or
    // increase the current number of running segments.
    def updateForceable(f: Int => Int): State = copy(forceable = f(forceable))

    private def updatedConsumers(state: State): Unit = {
      download.info.segments.setValue(state.segmentConsumers.size)
      download.info.activeSegments.setValue(state.segmentConsumers.count(_._2.started))
    }

    def addConsumer(consumer: ResponseConsumer, data: SegmentHandlerData): State = {
      val stateNew = copy(segmentConsumers = segmentConsumers + (consumer -> data))
      updatedConsumers(stateNew)
      stateNew
    }

    def removeConsumer(consumer: ResponseConsumer): State = {
      val stateNew = copy(segmentConsumers = segmentConsumers - consumer)
      updatedConsumers(stateNew)
      stateNew
    }

    def updateConsumerData(consumer: ResponseConsumer)(f: SegmentHandlerData => SegmentHandlerData): State = {
      val stateNew = segmentConsumers.get(consumer).map { data =>
        copy(segmentConsumers = segmentConsumers + (consumer -> f(data)))
      }.getOrElse(this)
      updatedConsumers(stateNew)
      stateNew
    }

    def readyTryCnx: (State, Option[TryCnxData]) = (copy(tryCnx = None), tryCnx)

    private def completeTryCnx: State = {
      // Complete any pending cnx attempt.
      tryCnx.foreach(_.done())
      copy(tryCnx = None)
    }

    def cancelTrySegment: State = {
      trySegment.foreach(_.cancel())
      completeTryCnx.copy(trySegment = None)
    }

    def resume(restart: Boolean): State = {
      if (restart) {
        resetErrors.copy(
          started = false,
          stopping = false,
          failed = None,
          segmentConsumers = Map.empty,
          maxSegments = None
        )
      } else {
        resetErrors.copy(
          stopping = false,
          failed = None,
          maxSegments = None
        )
      }
    }

  }

}

class FileDownloader(dlMngr: DownloadManager, dl: Download) extends Actor with StrictLogging {

  import FileDownloader._
  import context.dispatcher

  // See RFC 7233 for range requests

  override def receive: Receive = receive(State(dlMngr, dl, started = dl.info.isSizeDetermined))

  def receive(state: State): Receive = {
    case DownloadResume(restart) =>
      applyState(resume(state, restart))

    case TryResume =>
      applyState(tryResume(state))

    case DownloadStop =>
      applyState(stop(state, DownloadException(message = "Download stopped", stopped = true), abort = true))

    case TryConnection(promise) =>
      applyState(tryConnection(state, promise))

    case AddConnection =>
      applyState(trySegment(state, force = true))

    case RemoveConnection =>
      applyState(stopSegment(state))

    case TrySegment =>
      val (state1, tryCnx) = state.readyTryCnx
      applyState(trySegment(state1.cancelTrySegment, tryCnx = tryCnx))

    case SegmentStarted(consumer, response) =>
      applyState(segmentStarted(state, consumer, response))

    case SegmentDone(consumer, error) =>
      applyState(segmentDone(state, consumer, error))

    case Downloaded(range) =>
      downloaded(state, range)

    case Refresh =>
      applyState(refresh(state))
  }

  private def applyState(state: State): Unit = {
    context.become(receive(state))
  }

  /** Refreshes download (settings). */
  private def refresh(state: State): State = {
    // We are called because either:
    //  - the maximum number of segments changed
    //  - sites settings changed
    // Note: caller does not precisely pinpoint which downloads are concerned by
    // those changes, and trigger a refresh on all downloads.
    // We may be concerned either if:
    //  - we still use the general segment limit; because we did not encounter
    //    errors making us determine a specific maximum for this download
    //  - the site to which we are attached did change: our acquired connections
    //    needs to be 'migrated' to the new site

    // Recompute our maximum number of segments stored in 'info', so that it is
    // reflected in the UI.
    state.updateMaxSegments()

    // 'Migrate' connections information by re-acquiring them.
    // Circumvents any limit for this one special case.
    val segmentConsumers = state.segmentConsumers.map {
      case (consumer, data) =>
        // Acquire (forced) 'new' connection with refreshed settings.
        state.dlMngr.tryAcquireConnection(
          state.download,
          force = true,
          count = data.acquired.count,
          active = state.isActive
        ).toOption.flatten.map { acquired =>
          // Release the previously acquired one, effectively transferring it
          // to any new site when applicable.
          state.dlMngr.releaseConnection(data.acquired)
          consumer -> data.copy(acquired = acquired)
        }.getOrElse {
          // If we could not acquire a new connection (due to error), leave the
          // current one alone. This is however not supposed to happen.
          consumer -> data
        }
    }
    state.copy(segmentConsumers = segmentConsumers)
  }

  /**
   * Resumes download.
   *
   * Only resets the internal state: caller (download manager) is responsible
   * to trigger new connection attempt when applicable.
   */
  private def resume(state0: State, restart: Boolean): State = {
    val action = if (restart) {
      "re-starting"
    } else if (state0.started) {
      "resuming"
    } else {
      "starting"
    }
    val message = s"Download $action"
    val state = state0.resume(restart)
    logger.info(s"${state.download.context} $message")
    state.download.info.addLog(LogKind.Info, message)
    state
  }

  /**
   * Tries resuming download.
   *
   * This is an internal action in specific cases (enabling 'trustAll' after
   * SSL issue).
   * If the download can be resumed, request goes through the download manager
   * as if explicitly done by user.
   * Otherwise (download already 'running') try to (re)start a new segment.
   */
  private def tryResume(state: State): State = {
    if (state.download.canResume) {
      // The download manager does follow the downloads. So we need to go
      // through it to resume the download.
      dlMngr.resumeDownload(state.download.id, restart = false)
      state
    } else trySegment(state)
  }

  /**
   * Stops download.
   *
   * Cancels any pending new segment attempt.
   * Upon aborting, aborts each active segment. Otherwise let current segments
   * finish, only preventing new ones to be created.
   */
  private def stop(state0: State, ex: DownloadException, abort: Boolean): State = {
    val download = state0.download
    logger.info(s"${download.context} Download stopping")
    download.info.addLog(LogKind.Info, "Download stopping")

    val state = state0.cancelTrySegment.copy(
      stopping = true,
      failed = Some(ex)
    )
    if (state.segmentConsumers.isEmpty) {
      done(state)
    } else if (abort) {
      // Stop all segments
      state.segmentConsumers.keys.foldLeft(state) { (state, consumer) =>
        state.updateConsumerData(consumer)(_.abort())
      }
    } else {
      // Let segments finish (only prevent new segments)
      state
    }
  }

  /**
   * Takes into account downloaded range.
   *
   * Updates remaining ranges.
   */
  private def downloaded(state: State, range: SegmentRange): Unit = {
    state.download.info.remainingRanges match {
      case Some(remainingRanges) =>
        remainingRanges.remove(range)
        state.download.info.downloaded.setValue(remainingRanges.getRemovedLength)
        logger.debug(s"${state.download.context} Downloaded range=<$range>; remaining $remainingRanges")

      case None =>
        state.download.info.downloaded.setValue(range.end + 1)
    }
  }

  /**
   * Changes an active segment range start.
   *
   * Used after I/O flushing issue, so that the invalidated range (which is
   * considered not written) is now considered not part of any active range
   * and can picked up again when starting a new segment (as part of the
   * remaining ranges).
   */
  private def changeSegmentStart(state: State, consumer: ResponseConsumer, start: Long): State = {
    state.updateConsumerData(consumer) { data =>
      data.copy(range = data.range.copy(start = start))
    }
  }

  /**
   * Changes an active segment range end.
   *
   * Used when:
   *  - we now know the end of the segment (download size determined)
   *  - the active segment has been split in two to start a new one
   *  - a following segment (previously split from this one) failed to start
   *    and is given back to its original segment
   */
  private def changeSegmentEnd(state: State, consumer: ResponseConsumer, end: Long): State = {
    val data = state.segmentConsumers(consumer)
    val range0 = data.range
    val range = range0.copy(end = end)
    logger.info(s"${state.download.context(range0)} Change active segment range end from=<${range0.endStr}> to=<${range.endStr}>")
    data.responseConsumer.end = end
    state.updateConsumerData(consumer) { data =>
      data.copy(
        range = range,
        // Update original range if we got the actual end
        originalRange = if (range0.end < 0) range else data.originalRange
      )
    }
  }

  /** Tries to add a new connection (and thus start a new segment). */
  private def tryConnection(state: State, promise: Promise[Unit]): State = {
    val tryCnx = TryCnxData(promise)
    // Pending segment trying counts as connection trying.
    if (state.trySegment.isEmpty) {
      trySegment(state, tryCnx = Some(tryCnx))
    } else {
      // Use any previous attempt result.
      state.tryCnx match {
        case Some(previous) =>
          tryCnx.completeWith(previous)
          state

        case None =>
          state.copy(tryCnx = Some(tryCnx))
      }
    }
  }

  /**
   * Tries to start a new segment.
   *
   * If possible, or forced, selects a remaining range to download and start a
   * new segment to handle it.
   * May be a non-active one (not currently handled by active segments), or may
   * be split from an existing active segment.
   */
  private def trySegment(state0: State, force: Boolean = false, tryCnx: Option[TryCnxData] = None): State = {
    var state = state0
    val download = state.download
    // 'force' is whether we are (manually) asked to force connection.
    // 'forced' is whether new connection will be forced, which may also happen
    // automatically because the number of max segments was raised manually
    // (previous manually forced connection).
    // When forcing, don't count this connection relatively to limits.
    val forced = force || {
      val can = state.forceable > 0
      if (can) state = state.updateForceable(_ - 1)
      can
    }
    // We can only start a new segment if ranges are accepted and we did not yet
    // reached max number of segments (and a TrySegment is not currently
    // scheduled).
    val canTry = !state.stopping && (forced || (state.trySegment.isEmpty && !state.hasTooManyErrors))
    lazy val canAddSegment = {
      // Note: actually if size is unknown only one segment can be running.
      // This is taken care of when searching for a new segment to start.
      if (forced) true
      else {
        val limit = state.getMaxSegments
        val ok = state.getSegments < limit
        if (!ok) download.updateLastReason(Some(s"Limit reached: number of segments limit=<$limit>"))
        ok
      }
    }
    lazy val canStartSegment = canTry && canAddSegment
    val trying = if (download.acceptRanges.contains(true) && canStartSegment) {
      val r = trySegmentRange(state, download, force, forced, tryCnx)
      state = r._1
      r._2
    } else if (canTry && download.info.remainingRanges.isEmpty && state.segmentConsumers.isEmpty) {
      // Either this is the very first connection attempt, or the initial
      // request failed. So (re-)try it if possible.
      tryAcquireConnection(state, download, forced, tryCnx) match {
        case Left(s) =>
          state = s
          false

        case Right(acquiredOpt) =>
          // Some servers won't indicate explicit ranges support (Accept-Ranges
          // header) but still handle them when requested.
          // So if we don't already know the server does not handle ranges,
          // probe it by requesting whole content as a partial range request:
          // we will be able to determine support in all cases.
          val range =
            if (download.acceptRanges.contains(false)) SegmentRange.zero
            else SegmentRange.all
          acquiredOpt.exists { acquired =>
            state = segmentStart(state, range, acquired, force, forced, tryCnx)
            true
          }
      }
    } else {
      // We cannot try a new segment.
      // We also end up here if first connection request is ongoing (no response
      // yet); may happen if a TryConnection is triggered.
      if (canTry && !download.info.isSizeDetermined && state.segmentConsumers.nonEmpty) {
        // If the first connection is ongoing, link any new attempt to the
        // current one.
        // Note: the very first connection is expected to only be triggered
        // through a TryConnection, so there should be a previous ongoing
        // 'tryCnx'.
        val completedWith = for {
          nextAttempt <- tryCnx
          previousAttempt <- state.segmentConsumers.values.find(_.tryCnx.nonEmpty).flatMap(_.tryCnx)
        } yield {
          nextAttempt.completeWith(previousAttempt)
        }
        completedWith.nonEmpty
      } else {
        false
      }
    }

    if (!trying) {
      // For whatever reason, we did not try a new segment. When applicable,
      // consider this cnx attempt done.
      tryCnx.foreach(_.done())
    }
    state
  }

  /** Determines an available remaining range and starts a new segment. */
  private def trySegmentRange(state0: State, download: Download, force: Boolean, forced: Boolean, tryCnx: Option[TryCnxData])
    : (State, Boolean) =
  {
    // Notes:
    // Usually the currently active segments are expected to complete all
    // remaining ranges. Some active segments may not have yet started to
    // download. But if a previous segment failed, there may also be remaining
    // non-active ranges.
    // In either case the best candidate is the largest remaining range, taking
    // into account that if a range is being downloaded (active segment) we will
    // split it in two.
    var state = state0

    // Clone the current remaining ranges (we will work with it)
    val trying = download.info.remainingRanges.map(_.clone()) match {
      case Some(remainingRanges) if remainingRanges.getRanges.size > 0 =>
        val minSize = download.minSegmentSize
        val remaining = remainingRanges.getRanges
        // Get all active remaining ranges
        val remainingActive = state.segmentConsumers.toList.flatMap { case (consumer, data) =>
          val activeRange = data.range
          remaining.asScala.find { remainingRange =>
            // Find the remaining range that this active segment is working on
            ((remainingRange.start >= activeRange.start) && (remainingRange.start < activeRange.end)) ||
              ((remainingRange.start < activeRange.start) && (remainingRange.end > activeRange.start))
          }.map { remainingRange =>
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
        state.segmentConsumers.values.foreach { data =>
          remainingRanges.remove(data.range)
        }
        // Note: non-active segments are not subject to size limit
        val remainingNonActive = remainingRanges.getRanges.asScala.toList.map { range =>
          (Option.empty[ResponseConsumer], range, range.length)
        }

        // Get the best candidate
        val candidate = (remainingActive ::: remainingNonActive).sortBy(-_._3).headOption
        // If nothing matches, we were left with only too small active segments.
        if (candidate.isEmpty) download.updateLastReason {
          Some(s"Limit reached: no segment of minimum size=<${Units.storage.toHumanReadable(minSize)}>")
        }
        candidate.flatMap {
          case (consumerOpt, range, _) =>
            // Now that we know we really can start a new segment, ensure we can
            // actually create a new connection.
            tryAcquireConnection(state, download, forced, tryCnx) match {
              case Left(s) =>
                state = s
                None

              case Right(acquiredOpt) =>
                acquiredOpt.map { acquired =>
                  (consumerOpt, range, acquired)
                }
            }
        }.exists {
          case (consumerOpt, range, acquired) =>
            consumerOpt match {
              case Some(consumer) =>
                // This segment is 'active': a consumer owns it and we will need
                // to change its segment end so that a new consumer can now own
                // this new (sub)segment.
                //
                // We assume the request will succeed (most of the time) and we
                // can change the active segment end right now. In return if the
                // new consumer fails to start the segment, we will try to give
                // it back to the original owner (if still running).
                // The 'worst case' would be:
                //  1. We change consumerA segment end
                //  2. We start consumerB (segment start follows consumerA
                //     segment end)
                //  3. consumerA reaches its segment end and stops
                //  4. consumerB fails to actually start: its segment still need
                //     to be downloaded and we will need to start a new consumer
                //     since consumerA ended
                // However most issues upon starting a new segment would happen
                // with the first attempts (segments limit reached), and at this
                // point the segments to download are still large so there is
                // less chances for a consumer to end before another one fails.
                // In other words: it's not worth waiting for the new segment to
                // start before changing the previous segment end.
                val newRange = range.copy(
                  start = range.start + range.length / 2 + 1
                )
                state = changeSegmentEnd(state, consumer, newRange.start - 1)
                state = segmentStart(state, newRange, acquired, force, forced, tryCnx)

              case None =>
                // This segment is not 'active' (no consumer owns it) and we can
                // (try to) start right away.
                state = segmentStart(state, range, acquired, force, forced, tryCnx)
            }
            true
        }

      case Some(_) =>
        // Belt and suspenders: the download is actually finished, since there
        // is no more segment to download. We don't expect it to happen in
        // normal circumstances: download should properly end in success or
        // failure; but some bugs may prevent us to properly set the download
        // as ended.
        state = done(state)
        false

      case None =>
        // Size is unknown but ranges are accepted: we can only have one active
        // segment.
        if (state.segmentConsumers.isEmpty) {
          // We can only specify the range start, and continue downloading past
          // what we already downloaded.
          val range = SegmentRange(download.info.downloaded.get)
          tryAcquireConnection(state, download, forced, tryCnx) match {
            case Left(s) =>
              state = s
              false

            case Right(acquiredOpt) =>
              acquiredOpt.exists { acquired =>
                state = segmentStart(state, range, acquired, force, forced, tryCnx)
                true
              }
          }
        } else {
          false
        }
    }

    (state, trying)
  }

  /** Tries to acquire a new connection. */
  private def tryAcquireConnection(state: State, download: Download, forced: Boolean, tryCnx: Option[TryCnxData])
    : Either[State, Option[AcquiredConnection]] =
  {
    state.dlMngr.tryAcquireConnection(
      download,
      force = forced,
      count = !forced,
      active = state.isActive
    ).left.map { ex =>
      tryCnx.foreach(_.attemptFailure(ex))
      // If we failed because the target file does not exist (while it should)
      // we can stop right now.
      if (ex.cause.isInstanceOf[NoSuchFileException]) {
        stop(state, ex, abort = false)
      } else {
        val newState = state.addError(ex, downloaded = false)
        handleError(newState, aborted = false, Some(ex))
      }
    }
  }

  /** Starts a new segment. */
  private def segmentStart(state0: State, range: SegmentRange, acquired: AcquiredConnection,
    force: Boolean, forced: Boolean, tryCnx: Option[TryCnxData]
  ) : State = {
    var state = state0
    val download = state.download
    val uri = download.info.actualUri.get
    download.rateLimiter.addDownload()
    try {
      val message = s"Starting range=$range sslTrust=${acquired.sslTrust}"
      logger.info(s"${download.context(range)} $message")
      download.info.addLog(LogKind.Debug, message)

      // Upon resuming a new download (!started and downloaded already set),
      // only do a HEAD request to determine size, accept ranges, etc. The way
      // requests are handled, the response consumer will end upon receiving the
      // response, on which we update our state (actual remaining ranges etc),
      // and a new segment will be tried right away (starting from the already
      // downloaded offset).
      val downloaded = download.info.downloaded.get
      val request = state.dlMngr.newRequest(
        uri = uri,
        head = !state.started && (downloaded > 0),
        referrer = download.referrer,
        cookie = download.cookie,
        userAgent = download.userAgent,
        rangeValidator = download.info.rangeValidator,
        range = range
      )

      val requestProducer = new BasicAsyncRequestProducer(URIUtils.extractHost(uri), request)
      // Attach a context so that we can retrieve redirection URIs.
      // It happens that redirection URIs are stored in the HTTP context.
      // An alternative would be to add an interceptor:
      //   - in the shared client, and use the context, possibly to execute a
      //     specific callback
      //  or
      //   - in a one-shot client, with a specific callback to execute
      val context = HttpClientContext.create()
      val responseConsumer = new ResponseConsumer(self, download, range, request, context)
      val client = state.dlMngr.getClient(acquired.sslTrust)
      client.execute(requestProducer, responseConsumer, context, null)

      val data = SegmentHandlerData(
        range = range,
        originalRange = range,
        acquired = acquired,
        forced = forced,
        tryCnx = tryCnx,
        context = context,
        request = request,
        responseConsumer = responseConsumer
      )
      state = state.addConsumer(responseConsumer, data)
      // If we forced segment to start, cancel any pending attempt and reset
      // errors.
      if (force) state = state.cancelTrySegment.resetErrors
    } catch {
      case ex: Exception =>
        val message = s"Failed to start segment range=$range download: ${ex.getMessage}"
        logger.error(s"${download.context(range)} $message", ex)
        download.info.addLog(LogKind.Error, message, Some(ex))
        tryCnx.foreach(_.attemptFailure(ex))
        state = segmentDone(state, None, range, forced = forced, acquired, SegmentRange.zero, Some(ex))
    }

    state
  }

  /** Called when new segment did start (successfully). */
  private def segmentStarted(state0: State, consumer: ResponseConsumer, response: HttpResponse): State = {
    val download = state0.download
    val tryCnx = state0.segmentConsumers.get(consumer).flatMap(_.tryCnx)
    val state1 = if (state0.started) {
      // We don't need anything anymore, this serves only to trigger a new
      // segment start if possible.
      state0.resetCnxErrors
    } else {
      val contentLength = Http.getContentLength(response)
      val lastModified = Http.getLastModified(response)
      // Some servers won't indicate explicit ranges support (no Accept-Ranges
      // header), but properly respond to range request (status code and
      // Content-Range header).
      val acceptRanges = Http.handleBytesRange(response) || {
        (response.getStatusLine.getStatusCode == HttpStatus.SC_PARTIAL_CONTENT) &&
          Http.getContentRange(response).isDefined
      }
      val validator = if (acceptRanges) Http.getValidator(response) else None

      // Update actual URI when applicable, so that next requests will use it
      // directly.
      state0.segmentConsumers.get(consumer).flatMap { data =>
        Option(data.context.getRedirectLocations)
      }.foreach { redirectLocations =>
        if (!redirectLocations.isEmpty) {
          download.info.actualUri.set(redirectLocations.asScala.last)
          val message = s"Actual (redirected) uri=<${download.info.actualUri.get}>"
          logger.info(s"${download.context} $message")
          download.info.addLog(LogKind.Info, message)
        }
      }
      val message = s"Download${download.ipContext} contentLength=<$contentLength>${
        lastModified.map(v => s" lastModified=<$v>").getOrElse("")
      } acceptRanges=<$acceptRanges>${
        validator.map(v => s" validator=<$v>").getOrElse("")
      }"
      logger.info(s"${download.context} $message")
      download.info.addLog(LogKind.Info, message)

      if (contentLength < 0) {
        download.info.addLog(LogKind.Warning, "Download size is unknown")
        // maxSegments will automatically be 1 now
        state0.updateMaxSegments()
      }

      // Set size (if not already done by consumer)
      download.setSize(contentLength)
      download.info.remainingRanges = if (contentLength >= 0) Some(new SegmentRanges(contentLength)) else None
      download.info.rangeValidator = validator
      download.acceptRanges(acceptRanges)
      download.info.lastModified.set(lastModified.orNull)

      if (!acceptRanges) {
        download.info.addLog(LogKind.Warning, "Download resuming is not supported")
        // maxSegments will automatically be 1 now
        state0.updateMaxSegments()
      }

      // If not already done, save subtitle file.
      download.info.subtitle.foreach { subtitle =>
        subtitle.prepare(logger, download)
      }

      // If 'downloaded' is already set, we are resuming an existing file
      // assuming its current size was already downloaded.
      val downloaded = download.info.downloaded.get
      if (downloaded > 0) {
        download.info.remainingRanges.foreach { remainingRanges =>
          remainingRanges.remove(SegmentRange(0, downloaded - 1))
        }
      }
      val state1 = if (contentLength >= 0) changeSegmentEnd(state0, consumer, contentLength - 1) else state0
      state1.resetCnxErrors.copy(started = true)
    }
    val state2 = state1.updateConsumerData(consumer)(_.copy(started = true))
    // If the segment/connection was forced, and we went beyond the segment
    // count limit, keep this new limit in mind.
    // Only try a new segment when applicable.
    val segments = state2.getSegments
    if (state2.segmentConsumers(consumer).forced && (segments > state2.getMaxSegments)) {
      // When applicable consider the cnx attempt done, since we didn't try a
      // new segment.
      tryCnx.foreach(_.done())
      state2.setMaxSegments(segments)
    } else {
      // Keep on trying new cnx as we try a new segment.
      trySegment(state2, tryCnx = tryCnx.map(_.renew))
    }
  }

  /** Called when segment is done (successfully or with error). */
  private def segmentDone(state: State, consumer: ResponseConsumer, error: Option[Exception]): State = {
    state.segmentConsumers.get(consumer).map { data =>
      val downloaded = SegmentRange(data.range.start, consumer.position - 1)
      // Fail segment attempt when applicable.
      for {
        tryCnx <- data.tryCnx
        ex <- error
      } tryCnx.attemptFailure(ex)
      segmentDone(state.removeConsumer(consumer), Some(data), data.range, forced = data.forced, data.acquired, downloaded, error)
    }.getOrElse(state)
  }

  private def segmentDone(state0: State, dataOpt: Option[SegmentHandlerData], range: SegmentRange, forced: Boolean,
    acquired: AcquiredConnection, downloaded: SegmentRange, error: Option[Exception]
  ): State = {
    var state = state0
    val download = state.download
    state.dlMngr.releaseConnection(acquired)
    download.rateLimiter.removeDownload()
    // Take into account our failure if any.
    val downloadError: Option[DownloadException] = error.map {
      case ex0: DownloadException => ex0
      case ex0: Exception => DownloadException(message = ex0.getMessage, cause = ex0, started = dataOpt.exists(_.started))
    }
    // Note: aborting request should not have triggered an exception.
    val aborted = dataOpt.exists(_.aborted)
    var actualError: Option[DownloadException] = if (!aborted) {
      downloadError
    } else {
      None
    }

    // Remember when we can force another connection.
    if (forced && !aborted && !state.hasTooManyErrors) state = state.updateForceable(_ + 1)

    state = handleWriteError(state, downloadError)
    actualError.foreach { ex =>
      state = state.addError(ex, downloaded.isDefined)
    }
    val status = if (aborted) {
      "aborted"
    } else if (actualError.isDefined) {
      "failed"
    } else {
      "finished"
    }
    val downloadedRange = if (downloaded.isDefined) s" downloaded=$downloaded" else ""
    // Note: log IP address if segment failed upon start.
    val message0 = s"Segment $status${if (actualError.exists(!_.started)) download.ipContext else ""}"
    val message = s"${download.context(range)} $message0${
      actualError.map(v => s" ex=<$v>").getOrElse("")
    }$downloadedRange; remaining segments=${state.getSegments}"
    actualError match {
      case Some(ex) =>
        logger.error(message, ex)
        state.download.info.addLog(LogKind.Error, s"$message0: ${ex.getMessage}", Some(ex))

      case None =>
        logger.info(message)
        state.download.info.addLog(LogKind.Debug, s"$message0 range=$range$downloadedRange")
    }

    // Upon issue, and if range was not started, try to give it back to its
    // original owner.
    if (actualError.isDefined && download.info.remainingRanges.exists(_.contains(range.start))) {
      state.segmentConsumers.find {
        case (_, handlerData) =>
          // The range to give back must follow an handler end, and belong to
          // its original range.
          (range.start == handlerData.range.end + 1) && (range.end <= handlerData.originalRange.end)
      }.foreach {
        case (consumer, _) =>
          state = changeSegmentEnd(state, consumer, range.end)
      }
    }

    // Handle partial ranges support discovery failure.
    if (download.acceptRanges.isEmpty && actualError.exists(_.rangeFailed)) {
      val ex = actualError.get
      download.acceptRanges(false)
      download.info.addLog(LogKind.Warning, "Download resuming is not supported")
      // maxSegments will automatically be 1 now
      state.updateMaxSegments()
      // We can retry immediately, with ranges support disabled.
      actualError = Some(ex.copy(skipDelay = true))
    }

    // Handle any SSL error.
    var stopped = false
    var askPending = false
    if (actualError.exists(_.isSSLException)) {
      val ex = actualError.get
      acquired.sslErrorAsk match {
        case Some(ask) =>
          if (ask) {
            // Ask user. If SSL is to be trusted, re-try.
            askPending = true
            Main.controller.askOnSslError(acquired.site, acquired.host, ex).onComplete {
              case Success(true) => self ! TryResume
              case _ =>
            }
            // Trigger stopping if that was the last consumer
            if (state.segmentConsumers.isEmpty) {
              state = stop(state, ex, abort = false)
              stopped = true
            }
          }

        case None =>
          // Automatically trust SSL, but not for site if it is the default
          val msg = s"Enabling 'trustAll' for ${
            if (!acquired.isDefaultSite) s"site=<${acquired.site}>"
            else s"server=<${acquired.host}>"
          } after SSL issue"
          logger.info(msg)
          state.download.info.addLog(LogKind.Warning, msg)
          // Since we will now trust SSL for this server/site, don't apply the
          // delay before the next attempt.
          actualError = Some(ex.copy(skipDelay = true))
          if (!acquired.isDefaultSite) state.dlMngr.trustSslSiteConnection(acquired.site, trust = true)
          else state.dlMngr.trustSslServerConnection(acquired.site, acquired.host, trust = true)
      }
    }

    // Note:
    // Download is not fully finished if segments are still running or
    // remaining. If content length was unknown, we are done if the segment
    // finished without issue (and was not aborted).
    val finished = state.segmentConsumers.isEmpty && (
      state.stopping ||
        (download.info.remainingRanges.isEmpty && actualError.isEmpty && !aborted) ||
        download.info.remainingRanges.exists(_.getRanges.isEmpty)
      )
    state = if (stopped) {
      state
    } else if (finished) {
      done(state)
    } else {
      handleError(state, aborted, actualError)
    }

    // Now that we may have tried a new connection, give a chance to other
    // downloads to also try a new connection (if applicable).
    // If we were done, let the download manager (which asynchronously handles
    // the 'final' state) do it (especially after changing the state).
    if (!askPending && !finished) state.dlMngr.tryConnection(Some(download.id))

    state
  }

  /**
   * Handles write error.
   *
   * Invalidates concerned range by reintegrating it in remaining ranges.
   * Also updates concerned active segment range so that the invalidated range
   * can be picked up again by the next new segment.
   */
  private def handleWriteError(state: State, error: Option[DownloadException]): State = {
    val ranges = error.map(_.rangesWriteFailed).getOrElse(Nil)

    if (ranges.nonEmpty) {
      // Re-add failed ranges to remaining ones.
      state.download.info.remainingRanges.foreach { remainingRanges =>
        ranges.foreach(remainingRanges.add)
        state.download.info.downloaded.setValue(remainingRanges.getRemovedLength)
      }
      // Update active segments start so that failed ranges are not seen as
      // 'active' and can be more easily picked in trySegment.
      ranges.foldLeft(state) { (state, range) =>
        // 1. Loop over failed ranges
        state.segmentConsumers.values.filter { data =>
          // 2. Find active ranges that overlap with the failed range
          // Note: the 'worst case' is when consecutive handlers barely finished
          // (end of downloaded range sent, but 'SegmentDone' not yet received)
          // and another segment triggered the write failure for all those
          // ranges. In this case, more than one consumer are concerned by
          // the (merged) failed range.
          (data.range.start <= range.end) && (data.range.end >= range.start)
        }.foldLeft(state) { (state, data) =>
          // 3. Update the active range start
          changeSegmentStart(state, data.responseConsumer, math.min(data.range.end, range.end) + 1)
        }
      }
    } else {
      state
    }
  }

  /**
   * Handles connection or segment error.
   *
   * Trigger a new segment start unless there were too many errors.
   */
  private def handleError(state: State, aborted: Boolean, error: Option[DownloadException]): State = {
    if (!state.hasTooManyErrors) {
      segmentFinished(state, aborted, error)
    } else {
      tooManyErrors(state, error)
    }
  }

  private def segmentFinished(state: State, aborted: Boolean, error: Option[DownloadException]): State = {
    // We may try to start a new segment.
    // Skip delay if there was no error or we are requested to skip it.
    if (error.forall(_.skipDelay)) {
      // Don't try a new segment if this one was aborted, unless we are below
      // the limit (should be because limit is 1 and we aborted the last
      // active segment).
      if (!aborted || (state.getSegments < state.getMaxSegments)) {
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

  /**
   * Handles situation when there have been too many errors.
   *
   * If applicable, auto-determine the maximum number of segments for this
   * download.
   * Stops download if range validator did change.
   */
  private def tooManyErrors(state0: State, error: Option[DownloadException]): State = {
    // Too many issues.
    var state = state0
    val download = state.download
    error.foreach { ex =>
      // We won't automatically force other connections.
      state = state.updateForceable(_ => 0)
      if (ex.rangeFailed) {
        // If the range validator changed (file on server was changed since we
        // started ?), notify caller through promise, and let it decide whether
        // to end the download or restart it (from its current state).
        // Otherwise, that means the server actually does not support ranges
        // even though it advertises it.
        if (ex.rangeValidator != download.info.rangeValidator) {
          val message = s"Too many errors; range validator changed from=<${download.info.rangeValidator}> to=<${ex.rangeValidator}>"
          logger.warn(s"${download.context} $message")
          state.download.info.addLog(LogKind.Warning, message)
          state = stop(state, ex, abort = true)
        } else {
          download.acceptRanges(accept = false)
          // maxSegments will automatically be 1 now
          state.updateMaxSegments()
          val message = "Too many errors; assume server does not accept ranges"
          logger.warn(s"${download.context} $message")
          state.download.info.addLog(LogKind.Warning, message)
          // Trigger stopping if that was the last consumer
          if (state.segmentConsumers.isEmpty) state = stop(state, ex, abort = false)
        }
      } else {
        // If there is no concurrent segment, we cannot consider we reached a
        // limit on how many segments we can open in parallel.
        val canStop = if (!ex.started && state.segmentConsumers.nonEmpty) {
          // Request failed.
          // Assume we cannot create more segments than what we currently have.
          val maxSegments = state.getSegments
          val message = s"Too many errors; set maxSegments=<$maxSegments>"
          logger.warn(s"${download.context} $message")
          state.download.info.addLog(LogKind.Warning, message)
          // Now we don't consider our cnx error as a true error. Since we will
          // now respect the deduced segments limit, reset any previous cnx
          // error and obviously don't stop the download.
          state = state.resetCnxErrors.setMaxSegments(maxSegments)
          false
        } else {
          // Segment had an issue after successfully starting.
          val message = "Too many errors"
          logger.warn(s"${download.context} $message")
          state.download.info.addLog(LogKind.Warning, message)
          // Trigger download stopping: we won't automatically try new
          // connections, so we will need to properly consider the download
          // stopped once the last segment is done.
          true
        }
        // Trigger stopping when applicable
        if (canStop) state = stop(state, ex, abort = false)
      }
    }
    // else: no Exception; this segment finished without issue.
    // Note: stopping was already triggered when applicable.

    state
  }

  /** Stops a segment if any. */
  private def stopSegment(state0: State): State = {
    val segments = state0.getSegments
    if (segments == 1) {
      // When there is only one segment running, simply stop the download
      self ! DownloadStop
      state0
    } else {
      val state = if (segments > 0) {
        // Select the farthest segment, and abort it
        val data = state0.segmentConsumers.values.toList.maxBy(_.range.start)
        state0.updateConsumerData(data.responseConsumer)(_.abort())
      } else {
        state0
      }
      // Reduce the segments limit (relatively to the current number of segments)
      state.setMaxSegments(math.max(1, segments - 1))
    }
  }

  /**
   * Called when download is done, whether successfully or not.
   *
   * Flushes written data and updates file name/date upon success.
   */
  private def done(state0: State): State = {
    // Belt and suspenders: since we are done, ensure there is no more pending
    // 'trySegment', which is useful to check whether download is complete.
    var state = state0.cancelTrySegment
    val download = state.download
    val info = download.info
    val lastModified = Option(info.lastModified.get)

    // If we had a failure, but download actually completed (e.g. only one
    // segment could be started, and the whole download range was completed)
    // ignore the failure.
    if (state.isComplete && state.failed.nonEmpty) state = state.copy(failed = None)

    // Close the file and handle any issue.
    val closeError = try {
      download.closeFile(lastModified, done = state.failed.isEmpty)
      // Ensure path (still) exists when applicable.
      val path = download.downloadFile.getWorkingPath
      if (download.info.hasDownloaded && !Files.exists(path)) throw new NoSuchFileException(path.toString)
      None
    } catch {
      case ex: DownloadException =>
        Some(ex)

      case ex: Exception =>
        Some(DownloadException(
          message = s"I/O error: (${ex.getClass.getSimpleName}) ${ex.getMessage}",
          cause = ex,
          started = state.started
        ))
    }

    state = handleWriteError(state, closeError)
    state.failed.orElse(closeError) match {
      case Some(ex) if closeError.isEmpty || ex.stopped =>
        // The download did fail on its own.
        // Log any closing error (may happen upon stopping).
        closeError.foreach { ex =>
          logger.error(s"${download.context} Download uri=<${download.uri}> error=<${ex.getMessage}>", ex)
          info.addLog(LogKind.Error, s"Download error: ${ex.getMessage}", Some(ex))
        }
        info.promise.tryFailure(ex)
        // Do not stop the actor (we can still resume/restart)
        state

      case Some(ex) =>
        // The download was actually ok, but closing failed: we need to log the
        // closing error (not done yet) before deciding whether we try to resume
        // the download or leave it stopped in error.
        logger.error(s"${download.context} Download uri=<${download.uri}> error=<${ex.getMessage}>", ex)
        info.addLog(LogKind.Error, s"Download error: ${ex.getMessage}", Some(ex))
        if (state.isComplete || (state.stopping && !state.isActive)) {
          // The download actually completed: there is nothing more to download
          // and we should not try to resume it.
          // A possible case is when the download file has been externally
          // deleted, during or at the end of the download, not preventing the
          // file to be fully written, which is at least possible with POSIX
          // filesystems.
          // Trying to resume may at best let the download stuck in 'pending'
          // status, and at worst trigger a stack overflow or an infinite loop:
          // since there is no more segment to complete, download is again
          // considered 'done' and we are called back asynchronously or
          // recursively while we may trigger the same closing error again.
          //
          // Alternatively we are stopping and there is nothing else ongoing.
          info.promise.tryFailure(ex)
          state
        } else {
          // Re-try right now: ranges that could not be written will be done
          // again.
          segmentFinished(state, aborted = false, None)
        }

      case None =>
        // We are really done (success)
        logger.info(s"${download.context} Download uri=<${download.uri}> done: success${
          lastModified.map(v => s" (file date=<$v>)").getOrElse("")
        }")
        info.promise.trySuccess(())
        // Time to stop ourself, unless download is done with error (in which
        // case user has to restart or remove it explicitly).
        if (info.doneError.isEmpty) context.stop(self)
        state
    }
  }

}

class ResponseConsumer(
  downloadHandler: ActorRef,
  download: Download,
  range: SegmentRange,
  request: HttpRequestBase,
  context: HttpClientContext
) extends AbstractAsyncResponseConsumer[Unit] {

  private[mngr] var position: Long = range.start
  @volatile private[mngr] var end: Long = range.end

  // How many consecutive times we actually read nothing.
  private var withoutData = 0

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

  private def fail(ex: DownloadException): Unit = {
    // First set failure.
    failed(ex)
    // Then abort the request.
    // It is sometimes necessary: for some URIs, 'failed' is enough to stop
    // the request, for others the connection actually keeps on receiving
    // data (and stays alive in the background ...).
    request.abort()
  }

  override def onResponseReceived(response: HttpResponse): Unit = {
    // Through the context we can actually get hold of the underlying client
    // connection.
    if (!download.info.isInetAddressDetermined) {
      Option(context.getConnection(classOf[ManagedNHttpClientConnection])).foreach { cnx =>
        download.info.inetAddress.set(Some(cnx.getRemoteAddress))
      }
    }
    val statusLine = response.getStatusLine
    // Notes:
    // On the very first try we request a range "0-", before knowing whether
    // server supports returning ranges or not.
    // Server is expected to return proper 206 success code if supported, or
    // ignore the header and return standard 200 success code.
    // For this first try, we actually don't want to enforce partial range
    // correctness: 200 status code may indicate server does not support it,
    // and when supported we only see the range validator for the first time
    // (will be remembered by caller for later connections).
    // Depending on returned headers, caller will properly determine whether
    // server accept ranges.
    //
    // After first segment response, we know whether server supports ranges or
    // not.
    // When resuming a download from existing file, the download is seen as
    // "started" because content was already downloaded, but "acceptRanges"
    // is still unknown until first segment is tried. Since we don't know yet
    // whether server does support ranges, client behaves as for the first cnx.
    // This is also a first try for this new download entry.
    //
    // In case the server somehow understands we requested a range, does not
    // support this feature and instead responds with the dedicated 416 error
    // (Requested Range Not Satisfiable), report range failure to caller: it
    // will remember server does not support ranges before retrying.
    val firstCnx = !download.isStarted || download.acceptRanges.isEmpty
    val isRange = (position > 0) || range.isDefined
    val failure = if (statusLine.getStatusCode / 100 != 2) {
      // Request failed with HTTP code other than 2xx
      Some(DownloadException(
        s"Request failed with HTTP status=<(${statusLine.getStatusCode}) ${statusLine.getReasonPhrase}>",
        rangeFailed = isRange && (statusLine.getStatusCode == HttpStatus.SC_REQUESTED_RANGE_NOT_SATISFIABLE)
      ))
    } else if (!firstCnx && isRange) {
      // We requested a partial content; ensure the response is consistent.
      // It's the server responsibility to only return the appropriate HTTP code
      // when all is fine. We can still ensure that we get what we requested.
      val rangeValidator = Http.getValidator(response)
      val contentRange = Http.getContentRange(response)
      if (statusLine.getStatusCode != HttpStatus.SC_PARTIAL_CONTENT) {
        Some(DownloadException(
          message = s"Range request not honored; HTTP status=<(${statusLine.getStatusCode}) ${statusLine.getReasonPhrase}>",
          rangeFailed = true,
          rangeValidator = rangeValidator
        ))
      } else if (download.info.rangeValidator != rangeValidator) {
        Some(DownloadException(
          message = s"Range request validator mismatch: expected=<${
            download.info.rangeValidator.getOrElse("")
          }> actual=<${
            rangeValidator.getOrElse("")
          }>",
          rangeFailed = true,
          rangeValidator = rangeValidator
        ))
      } else if (contentRange.exists(!_.matches(range))) {
        Some(DownloadException(
          message = s"Range request content mismatch: expected=<$range> actual=<${
            contentRange.getOrElse("")
          }>",
          rangeFailed = true,
          rangeValidator = rangeValidator
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
      case Some(ex) =>
        fail(ex)

      case None =>
        // When applicable it's better to preallocate the file size before
        // writing anything. So set the download size if it's our role (we must
        // be first, and not doing a ranged request).
        if ((position == 0) && (!range.isDefined || range.isInfinite)) {
          download.setSize(Http.getContentLength(response))
        }
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
    def loop(first: Boolean): Boolean = {
      // Use hint when rate is limited
      val count0 = if ((end >= 0) && (end != Long.MaxValue)) end + 1 - position else Long.MaxValue
      val count = math.min(count0, download.rateLimiter.getReadSizeHint)
      // Zero copy: transfer received content directly to file.
      // This require to do it right now: if we delegate, say to the parent
      // actor, we will be called again (decoder not empty) at least until
      // decoder is processed (assuming the decoder is thread-safe).
      val transferred = try {
        download.downloadFile.write(decoder, position, count, downloadHandler)
      } catch {
        case ex: DownloadException =>
          fail(ex)
          0L

        case ex: Exception =>
          fail(DownloadException(
            message = s"I/O error: (${ex.getClass.getSimpleName}) ${ex.getMessage}",
            cause = ex,
            started = true
          ))
          0L
      }
      if (transferred > 0) {
        withoutData = 0
        position += transferred
        download.rateLimiter.consumeTokens(transferred)

        // Check whether we are done with this
        val endReached = (end >= 0) && (position > end)
        // Do not keep on consuming the decoder if rate is limited.
        // If data remain, we will be called again, which let some more chances
        // for other downloads to consume too.
        // Leave loop if transferred size is lower than requested; usually means
        // there is nothing more buffered (at worst we will be called again).
        // Note: in some versions of Apache NIO (core < 4.4.12) the actual
        // number of transferred bytes may exceed the requested limit.
        if (!endReached && !download.rateLimiter.isLimited && (transferred >= count)) loop(first = false)
        else endReached
      } else {
        // No (more) data received from content decoder.
        // In some cases (usually after a period of rate limiting, i.e.
        // suspending/resuming) the content decoder may remain stuck: content
        // is considered received but reading it returns nothing. Sometimes the
        // next time we are called data are available; otherwise it does not
        // seem possible to unblock the situation. Leave a few chances for data
        // to be available, then properly fail the consumer.
        if (withoutData > 10) {
          fail(DownloadException(
            message = "I/O error: no data was read",
            started = true
          ))
        } else if (first) {
          // This was the first loop: there really was nothing to read.
          withoutData += 1
        }
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
      val endReached = loop(first = true)
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
    // shutdowns triggers a "Connection closed unexpectedly" error. But we
    // don't care about errors as long as we reached the segment end.
    Option(getException) match {
      case Some(ex) if (end < 0) || (position < end) =>
        downloadHandler ! FileDownloader.SegmentDone(this, Some(ex))

      case _ =>
        downloadHandler ! FileDownloader.SegmentDone(this, None)
    }
  }

}
