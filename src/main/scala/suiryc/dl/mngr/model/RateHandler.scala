package suiryc.dl.mngr.model

import java.util.{LinkedList ⇒ jLinkedList}
import scala.concurrent.duration._

object RateHandler {

  case class TimedValue(time: Long, var value: Long)

}

/**
 * Helper to compute live download rate.
 *
 * Time is divided in slices as for the rate limiter. The amount of downloaded
 * data is computed for the current slice each time the caller updates the
 * (total) downloaded size.
 * For smoother rate value changes, the following is done:
 *  - the rate is computed by taking into account multiple slices (spanning over
 *    the requested duration)
 *  - the oldest slice is only taken into account partially (depending on its
 *    start time relatively to the requested computation duration)
 *  - the time slice duration is aligned on the one used for rate limiting
 *  - when rate is limited, the current (ongoing) time slice is not taken into
 *    account: we assume that in the worst case data are downloaded at the start
 *    of the slice and download is paused for the rest of the slice
 * Also for better computation, it is recommended for caller to at least update
 * downloaded size once per time slice.
 *
 * Note: beware that computing the rate from 'downloaded' data (as read from the
 * content decoder) may result in a misleading 'burst' after disabling rate
 * limiting: the underlying connection has received data (buffered) but rate
 * limiting does limit how many we actually process per time period, so when
 * removing the limit we suddenly drain the buffer which artificially increases
 * the rate (possibly exceeding the physical limit of the connection).
 *
 * @param rateLimiter the download rate limiter
 * @param startValue the current downloaded size
 * @param duration time duration over which the rate is computed
 */
class RateHandler(rateLimiter: RateLimiter, startValue: Long, duration: FiniteDuration) {

  import RateHandler._

  // Period over which to compute the rate
  private val durationMs = duration.toMillis
  // Tie slice duration
  private val timeSliceMs = rateLimiter.timeSlice
  val timeSlice: FiniteDuration = timeSliceMs.millis

  // Slices
  private val history = new jLinkedList[TimedValue]()
  // Current downloaded size
  private var lastValue = startValue
  // Cached computed rate
  private var cachedRate = 0L

  def currentRate: Long = cachedRate

  /** Updates downloaded size. */
  def update(value: Long): Long = this.synchronized {
    // Adjust time according to requested time slice precision
    val now = System.currentTimeMillis
    val time = now - (now % timeSliceMs)
    // Compute delta from previous value and update history.
    // Force 0 upon negative delta (flushing error).
    val delta = math.max(0, value - lastValue)
    val last = history.peekLast
    if ((last != null) && (last.time == time)) last.value += delta
    else history.add(TimedValue(time, delta))
    lastValue = value

    // Drop old time slices from history, determine history start and how many
    // data were downloaded during that time.
    // When necessary (smoother value changes) do not include the last (current
    // and ongoing) time slice but only take into account past (finished) time
    // slices. This basically mean to care about adjusted 'time' and not 'now'.
    val limit = time - durationMs
    val limitReal = now - durationMs
    val it = history.iterator()
    @scala.annotation.tailrec
    def loop(start: Long, downloaded: Long): (Long, Long) = {
      if (it.hasNext) {
        val data = it.next()
        if (data.time < limit) {
          // Drop old time slices
          it.remove()
          loop(start, downloaded)
        } else if (rateLimiter.isLimited && (data.time == time)) {
          // Exclude the current/ongoing time slice
          (start, downloaded)
        } else {
          // Include this time slice.
          // If it started before the duration over which we compute the rate,
          // take it into account partially.
          if (data.time < limitReal) {
            val dataDuration = timeSliceMs - (limitReal - data.time)
            val value = (data.value * dataDuration) / timeSliceMs
            loop(math.min(start, limitReal), downloaded + value)
          } else {
            loop(math.min(start, data.time), downloaded + data.value)
          }
        }
      } else (start, downloaded)
    }
    val (start, downloaded) = loop(time, 0)
    val duration =
      if (rateLimiter.isLimited) time - start
      else now - start
    val delay = math.max(duration, timeSliceMs)
    cachedRate = downloaded * 1000 / delay
    cachedRate
  }

}
