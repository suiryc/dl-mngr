package suiryc.dl.mngr.util

import com.typesafe.scalalogging.StrictLogging
import java.net.URI
import java.util.Date
import org.apache.http.client.utils.DateUtils
import org.apache.http.{HttpHeaders, HttpResponse}

/** HTTP helpers. */
object Http extends StrictLogging {

  /**
   * Gets URI from string.
   *
   * String is cleaned when necessary.
   */
  def getURI(s: String): URI = {
    // "[]" are not allowed inside path components
    val cleaned = {
      // Format: http://server:port/path?query
      // First separate path (and query) from start of URI
      val parts = s.trim.split("/", 4)
      // Leave start untouched
      val start = parts.take(3).mkString("/")
      // Don't fail if there is no path
      val restOpt = parts.drop(3).headOption.map { v =>
        // Now separate path and query
        val parts = v.split("""\?""", 2)
        // Replace characters in path
        val path = parts.head.replaceAll("""\[""", "%5B").replaceAll("""\]""", "%5D")
        // Leave query untouched
        val query = parts.tail.headOption
        // Re-build path and query
        (List(path) ::: query.toList).mkString("?")
      }
      // Re-build URI
      (List(start) ::: restOpt.toList).mkString("/")
    }
    URI.create(cleaned)
  }

  /** Gets filename from URI. */
  def getFilename(uri: URI): String = {
    val parts = Option(uri.getPath).map(_.split('/').toList).getOrElse(Nil)
    if (parts.nonEmpty) parts.reverse.head
    else ""
  }

  /**
   * Gets content length from response headers.
   *
   * @return content length, or -1 if unknown (missing or not parsable)
   */
  def getContentLength(response: HttpResponse): Long = {
    Option(response.getFirstHeader(HttpHeaders.CONTENT_LENGTH)).map { h =>
      try {
        h.getValue.toLong
      } catch {
        case ex: Exception =>
          logger.error(s"Failed to parse HTTP content length=<${h.getValue}>: ${ex.getMessage}", ex)
          -1L
      }
    }.getOrElse(-1L)
  }

  /**
   * Gets whether server handles bytes range.
   *
   * Content length must be known and server return "Accept-Ranges: bytes"
   * header.
   */
  def handleBytesRange(response: HttpResponse, contentLength: Long): Boolean = {
    // We don't handle bytes range if either:
    //  - content length is unknown
    //  - server does not "Accept-Ranges: bytes"
    (contentLength >= 0) && Option(response.getFirstHeader(HttpHeaders.ACCEPT_RANGES)).map(_.getValue).contains("bytes")
  }

  /**
   * Gets HTTP range validator.
   *
   * See RFC7233 section 3.2.
   * We use either a non-weak entity-tag, or fallbacks to "Last-Modified" date.
   */
  def getValidator(response: HttpResponse): Option[String] = {
    // RFC 7233 section 3.2
    // A client MUST NOT generate an If-Range header field containing an
    // entity-tag that is marked as weak.
    Option(response.getFirstHeader(HttpHeaders.ETAG)).map(_.getValue).filterNot(_.startsWith("W/")).orElse {
      Option(response.getFirstHeader(HttpHeaders.LAST_MODIFIED)).map(_.getValue)
    }
  }

  /**
   * Gets "Last-Modified" date.
   *
   * @return date, or None if unknown (missing or not parsable)
   */
  def getLastModified(response: HttpResponse): Option[Date] = {
    Option(response.getFirstHeader(HttpHeaders.LAST_MODIFIED)).flatMap { h =>
      try {
        Some(DateUtils.parseDate(h.getValue))
      } catch {
        case ex: Exception =>
          logger.error(s"Failed to parse HTTP date=<${h.getValue}>: ${ex.getMessage}", ex)
          None
      }
    }
  }

}
