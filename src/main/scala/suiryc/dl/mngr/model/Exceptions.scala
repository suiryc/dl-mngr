package suiryc.dl.mngr.model

trait ApplicativeException extends Exception

case class DownloadException(
  message: String = null,
  cause: Throwable = null,
  /** Whether this download actually started. */
  started: Boolean = false,
  /** Whether stopping was requested. */
  stopped: Boolean = false,
  /** Whether this issue is triggered because download is being reused. */
  reused: Boolean = false,
  /** Whether the issue is due to ranges support. */
  rangeFailed: Boolean = false,
  /** The actual range validator that caused the issue. */
  rangeValidator: Option[String] = None,
  /** Segment ranges that could not be written. */
  rangesWriteFailed: List[SegmentRange] = Nil
) extends Exception(message, cause)
