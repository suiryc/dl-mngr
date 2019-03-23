package suiryc.dl.mngr.model

import suiryc.dl.mngr.Main

/** Rate limiter. */
class RateLimiter(private var _bytesPerSecond: Long) {

  // Implementation:
  // The resource is abstracted as 'tokens' (bytes in this case).
  // Time is divided in slices, and each slice has a given number of available
  // tokens (direct relation to target rate).
  // A download wishing to read incoming data first checks whether there still
  // are tokens available. If there is none, it 'sleeps' until the next slice,
  // otherwise it reads data (buffer size hint) and consumes the tokens.
  // If too many tokens were consumed, caller is not blocked, but next downloads
  // shall wait until next slice (see above), which starts accounting for the
  // excess amount of tokens consumed.
  //
  // For smoother limiting, time slices must not be too big (at most 1s), and
  // cannot be too small (triggers too much pause/resume in downloads activity).
  // The value used is 200ms.
  // We also adjust slice start to be a multiple of time slice, so that we can
  // easily determine when the current slice started (e.g. to display live dl
  // rate).

  /** Whether rate is actually limited. */
  private var _isLimited: Boolean = false
  /** How many downloads are using this limiter. */
  private var downloads: Int = 0
  /** How many slices per second. */
  private val slicesPerSecond: Long  = 5
  /** Time slice duration (ms). */
  val timeSlice: Long = 1000 / slicesPerSecond
  /** How many tokens (bytes) are allocated per time slice. */
  private var tokensPerSlice: Long = Long.MaxValue
  /** When did the 'current' time slice started. */
  private var sliceStart: Long = adjustSliceStart(System.currentTimeMillis)
  /** How many tokens (bytes) are still available in the 'current' time slice. */
  private var availableTokens: Long = Long.MaxValue

  /** Adjust slice start to be a multiple of the time slice. */
  private def adjustSliceStart(v: Long): Long = v - (v % timeSlice)

  /** Time slice end. */
  private def sliceEnd: Long = sliceStart + timeSlice

  /** Gets rate limit (0 if unlimited). */
  def bytesPerSecond: Long = _bytesPerSecond

  /** Gets whether rate is actually limited. */
  def isLimited: Boolean = _isLimited

  /** Changes rate limit (<= 0 for unlimited). */
  def setBytesPerSecond(bytesPerSecond: Long): Unit = this.synchronized {
    // Normalize to 0 for unlimited rate.
    _bytesPerSecond = math.max(0, bytesPerSecond)
    _isLimited = _bytesPerSecond > 0
    // Set sane values for unlimited rate.
    tokensPerSlice = if (_isLimited) _bytesPerSecond / slicesPerSecond else Long.MaxValue
    // Do not blindly reset availableTokens to tokensPerSlice, but also
    // consider currently availableTokens to smooth rate changing for the
    // current time slice.
    // When going from unlimited to limited: really use tokensPerSlice.
    // Otherwise, when staying limited: consume the remaining tokens for the
    // current time slice up to the new tokensPerSlice value. This smoothes
    // rate change on the current time slice, and in most cases prevents a
    // burst (e.g. giving full tokensPerSlice when most tokens were already
    // consumed).
    availableTokens = if (_isLimited) math.min(availableTokens, tokensPerSlice) else Long.MaxValue
  }
  // Compute initial values.
  setBytesPerSecond(_bytesPerSecond)

  /** Adds a download using this limiter. */
  def addDownload(): Unit = this.synchronized {
    downloads += 1
  }

  /** Removes a download using this limiter. */
  def removeDownload(): Unit = this.synchronized {
    downloads -= 1
  }

  /** Read size hint. */
  def getReadSizeHint: Long = this.synchronized {
    if (_isLimited) {
      math.max(
        tokensPerSlice / (2 * math.max(downloads, 1)),
        Main.settings.bufferReadMin.get
      )
    } else Long.MaxValue
  }

  /** Gets whether tokens are currently available. */
  def getAvailableTokens: Long = this.synchronized {
    if (_isLimited) {
      checkSlice(System.currentTimeMillis)
      availableTokens
    } else Long.MaxValue
  }

  /** How many milliseconds until the next time slice. */
  def nextSliceDelay: Long = this.synchronized {
    math.max(sliceEnd - System.currentTimeMillis, 0L)
  }

  /** Consume the given number of tokens. */
  def consumeTokens(tokens: Long): Long = this.synchronized {
    if (_isLimited) {
      checkSlice(System.currentTimeMillis)
      availableTokens -= tokens
      availableTokens
    } else Long.MaxValue
  }

  // Check current time slice is still ongoing, or setup the actual one.
  // Note: caller is responsible to call us only when rate is limited.
  private def checkSlice(now: Long): Unit = {
    @scala.annotation.tailrec
    def loop(): Unit = {
      if (now >= sliceEnd) {
        // Current time slice already ended, get to the new one.
        // Take into account that a new time slice gives us new tokens to
        // consume, and handle the case where more tokens than available
        // were consumed in a previous time slice.
        if (availableTokens < tokensPerSlice) {
          // Advance one slice, and update available tokens count.
          sliceStart += timeSlice
          availableTokens = math.min(tokensPerSlice, availableTokens + tokensPerSlice)
          loop()
        } else {
          // There is no more available token to get, so go to the actual time
          // slice in one step.
          sliceStart = now - ((now - sliceStart) % timeSlice)
          availableTokens = tokensPerSlice
        }
      }
      // else: current time slice still active
    }

    loop()
  }

}
