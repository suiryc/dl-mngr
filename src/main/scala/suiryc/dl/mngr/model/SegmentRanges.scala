package suiryc.dl.mngr.model

import java.util.{LinkedList ⇒ jLinkedList}

/** Segment range. */
case class SegmentRange(start: Long, end: Long) {
  def length: Long = end + 1 - start
  override def toString: String = s"[$start,$end]"
}

object SegmentRange {
  val zero = SegmentRange(0, -1)
}

/** Segment ranges. */
class SegmentRanges(val total: Long, full: Boolean = true) {

  // Note: a LinkedList is used because it is faster to manipulate. But it is
  // used in an immutable way; once built, it is replaced, not modified.
  protected var ranges: jLinkedList[SegmentRange] = new jLinkedList()
  if (full) ranges.add(SegmentRange(start = 0, end = total - 1))

  /** Gets segment ranges. */
  def getRanges: jLinkedList[SegmentRange] = ranges

  /** Gets ranges length. */
  def getRangesLength: Long = {
    var length = 0L
    val it = ranges.iterator
    @scala.annotation.tailrec
    def loop(): Unit = {
      if (it.hasNext) {
        val range = it.next
        length += range.length
        loop()
      }
    }
    loop()
    length
  }

  /** Gets removed length. */
  def getRemovedLength: Long = total - getRangesLength

  /** Whether a given position (offset) belongs to the ranges. */
  def contains(position: Long): Boolean = {
    val it = ranges.iterator
    @scala.annotation.tailrec
    def loop(): Boolean = {
      if (it.hasNext) {
        val range = it.next
        if ((position >= range.start) && (position <= range.end)) true
        else loop()
      } else false
    }
    loop()
  }

  /** Clears ranges. */
  def clear(): Unit = {
    ranges.clear()
  }

  /** Removes a range from the ranges. */
  def remove(other: SegmentRange): Unit = {
    val rangesNew = new jLinkedList[SegmentRange]()
    val it = ranges.iterator
    @scala.annotation.tailrec
    def loop(): Unit = {
      if (it.hasNext) {
        val range = it.next
        if (((other.start >= range.start) && (other.start <= range.end)) ||
          ((other.start < range.start) && (other.end >= range.start))) {
          // Remove from this range
          if (range.start < other.start) rangesNew.add(range.copy(end = other.start - 1))
          if (other.end < range.end) rangesNew.add(range.copy(start = other.end + 1))
        } else {
          // Range not concerned
          rangesNew.add(range)
        }
        loop()
      }
    }
    loop()
    ranges = rangesNew
  }

  /** Adds a range to the ranges. */
  def add(other: SegmentRange): Unit = {
    // The current ranges are properly ordered, so what remain is to determine
    // where to insert the other range, and whether there are ranges overlapping
    // with this other range (i.e. merging needed).
    // To handle merging, we iterate over ranges and work on the 'current' range
    // while going to the 'next' one. It's time to process the other range when
    // it starts before the end of the current one.
    val rangesNew = new jLinkedList[SegmentRange]()
    val it = ranges.iterator
    @scala.annotation.tailrec
    def loop(current: SegmentRange, other: SegmentRange): Unit = {
      def addRange(r: SegmentRange): Unit = {
        if (r.length > 0) rangesNew.add(r)
        ()
      }
      // First check whether the other range is to be treated with the current range.
      // Otherwise go to the next range.
      if ((other.length > 0) && (other.start <= current.end + 1)) {
        // Add the other range now
        if (other.end + 1 < current.start) {
          // Other range is separated from current range
          rangesNew.add(other)
          loop(current, SegmentRange.zero)
        } else {
          // Other range is merged with current range
          loop(SegmentRange(math.min(other.start, current.start), math.max(other.end, current.end)), SegmentRange.zero)
        }
      } else if (it.hasNext) {
        val range = it.next
        if (current.end + 1 < range.start) {
          // Current range is separated from next range
          addRange(current)
          loop(range, other)
        } else {
          // Current range is merged with next range
          loop(current.copy(end = math.max(current.end, range.end)), other)
        }
      } else {
        // Done. Include current range and other range when applicable.
        // Since we are here, we know the other range starts after the current
        // range (if both exist).
        addRange(current)
        addRange(other)
      }
    }

    val normalized = other.copy(start = math.max(0, other.start), end = math.min(total - 1, other.end))
    loop(SegmentRange.zero, normalized)
    ranges = rangesNew
  }

  override def toString: String = {
    val sb = new StringBuilder()
    sb.append(s"ranges=<$getRangesLength/$total>=<")
    val it = ranges.iterator
    @scala.annotation.tailrec
    def loop(first: Boolean): Boolean = {
      if (it.hasNext) {
        val range = it.next
        if (!first) sb.append(',')
        sb.append(range.toString)
        loop(false)
      } else false
    }
    loop(true)
    sb.append(s"/[0,$total]>")
    sb.toString
  }

  override def clone(): SegmentRanges = {
    val cloned = new SegmentRanges(total)
    cloned.ranges = ranges
    cloned
  }

}
