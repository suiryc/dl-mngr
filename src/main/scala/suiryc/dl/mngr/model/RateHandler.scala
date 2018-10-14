package suiryc.dl.mngr.model

import java.util.{LinkedList ⇒ jLinkedList}
import scala.concurrent.duration.FiniteDuration

object RateHandler {

  case class TimedValue(time: Long, var value: Long)

}

class RateHandler(startValue: Long, duration: FiniteDuration, val step: FiniteDuration) {

  import RateHandler._

  private val durationMs = duration.toMillis
  private val stepMs = step.toMillis

  private val history = new jLinkedList[TimedValue]()
  private var lastValue = startValue

  def update(value: Long): Long = this.synchronized {
    // Adjust time according to requested 'step' precision
    val now = System.currentTimeMillis
    val time = now - (now % stepMs)
    // Compute delta from previous value and update history.
    // Force 0 upon negative delta (flushing error).
    val delta = math.max(0, value - lastValue)
    val last = history.peekLast
    if ((last != null) && (last.time == time)) last.value += delta
    else history.add(TimedValue(time, delta))
    lastValue = value

    // Drop old time slices from history, determine history start and how many
    // data were downloaded during that time.
    // For smoother rate value changes, do not include the last (current and
    // ongoing) time slice but only take into account past (finished) time
    // slices. This basically mean to care about adjusted 'time' and not 'now'.
    val limit = time - durationMs
    val it = history.iterator()
    @scala.annotation.tailrec
    def loop(start: Long, downloaded: Long): (Long, Long) = {
      if (it.hasNext) {
        val data = it.next()
        if (data.time < limit) {
          // Drop old time slices
          it.remove()
          loop(start, downloaded)
        } else if (data.time == time) {
          // Exclude the current/ongoing time slice
          (start, downloaded)
        } else {
          // Include this time slice
          loop(math.min(start, data.time), downloaded + data.value)
        }
      } else (start, downloaded)
    }
    val (start, downloaded) = loop(time, 0)
    val delay = math.max(time - start, stepMs)
    downloaded * 1000 / delay
  }

}
