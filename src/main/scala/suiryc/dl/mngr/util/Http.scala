package suiryc.dl.mngr.util

import com.typesafe.scalalogging.StrictLogging
import java.net.{URI, URLDecoder}
import java.util.Date
import org.apache.http.client.utils.DateUtils
import org.apache.http.entity.ContentType
import org.apache.http.{HttpHeaders, HttpResponse}
import suiryc.dl.mngr.model.SegmentRange

/** HTTP helpers. */
object Http extends StrictLogging {

  case class ContentRange(start: Long, end: Long, total: Long) {

    def isInfinite: Boolean = total == Long.MaxValue

    def matches(range: SegmentRange): Boolean = {
      (start == range.start) &&
        range.isInfinite || (end == range.end)
    }

    override def toString: String = s"$start-$end/${if (isInfinite) "*" else total}"

  }

  private val CONTENT_RANGE_REGEXP = """\s*bytes\s+([\d]+)-([\d]+)/([*\d]+)\s*""".r

  /**
   * Gets URI for host.
   *
   * If string starts with a scheme, it is parsed as an URI.
   * Otherwise we assume it points to the hostname (optional port) and we use
   * the HTTP scheme by default.
   */
  def getHostURI(s: String): URI = {
    if (s.toLowerCase.matches("""^[-a-zA-Z_0-9]+://.*""")) {
      // This is an URL
      URI.create(s)
    } else {
      // Assume we only have the host, and use HTTP scheme by default.
      URI.create(s"http://$s")
    }
  }

  /**
   * Gets URI from string.
   *
   * String is cleaned when necessary.
   */
  def getURI(s: String): URI = {
    // Some characters (e.g. "[]") are not allowed inside path components and must be escaped
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
        // If we need to escape all characters (*WARNING*: we need to be sure the URL was not already escaped ...)
        //val path = parts.head.split('/').map(URLEncoder.encode(_, StandardCharsets.UTF_8)).mkString("/")
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

  /** Gets filename from String, dropping any hierarchy part. */
  def getFilename(s: String): String = s.split("\\|/").last

  /** Gets filename from HTTP response. */
  def getFilename(response: HttpResponse): Option[String] = {
    // Notes:
    // Content-Disposition 'filename' is preferred over Content-Type 'name'.
    // And RFC 8187 encoded parameter (for non-ASCII characters) is preferred
    // over standard (plain US-ASCII) parameter.
    //
    // With HTTP, we only expect either a standard parameter (e.g. 'name') or
    // an RFC 8187 encode value (e.g. 'name*', emphasis on the trailing '*').
    // Unlike MIME, HTTP does not allow splitting the value over multiple
    // parameters. e.g. 'name*0', 'name*1', ... or 'name*0*', 'name*1*', ...
    // would split a plain or encoded (if there is a trailing '*') value in
    // MIME, but are not defined (and thus not expected) in HTTP.
    Option(response.getFirstHeader("Content-Disposition")).flatMap { h =>
      h.getElements.toList.flatMap { element =>
        Option(element.getParameterByName("filename*")).toList.map { p =>
          decodeString(p.getValue, rfc8187 = true)
        } ::: Option(element.getParameterByName("filename")).map { p =>
          decodeString(p.getValue, rfc8187 = false)
        }.toList
      }.headOption
    }.orElse {
      val contentType = Option(ContentType.get(response.getEntity))
      contentType.flatMap { ct =>
        Option(ct.getParameter("name*"))
      }.map { p =>
        decodeString(p, rfc8187 = true)
      }.orElse {
        contentType.flatMap { ct =>
          Option(ct.getParameter("name"))
        }.map { p =>
          decodeString(p, rfc8187 = false)
        }
      }
    }
  }

  /**
   * Decodes string parameter value.
   *
   * Standard parameter string value is expected to be either an atom (no
   * special characters) or a quoted (characters can be escaped, but only '"'
   * is expected to be).
   * RFC 8187 encoded value is expected to contain a charset (usually UTF8), an
   * optional language tag and an URL-encoded (percent encoding) value, each
   * separated by the "'" (quote) character.
   *
   * @param s string to decode
   * @param rfc8187 whether this is a RFC 8187 encoded value
   * @return decoded string, possibly equal to the given value
   */
  def decodeString(s: String, rfc8187: Boolean): String = {
    val cleaned = s.trim()
    if (rfc8187) {
      // RFC 8187 value: decode with indicated charset.
      val split = cleaned.split("'", 3)
      // If the format or encoding is invalid, just return the original value.
      if (split.length < 3) cleaned
      else try {
        URLDecoder.decode(split(2), split(0))
      } catch {
        case _: Exception => cleaned
      }
    } else if (cleaned.startsWith("\"")) {
      // Quoted value.
      val chars = cleaned.toCharArray
      val charsLength = chars.length
      def loop(offset: Int, unquoted: String): String = {
        if (offset >= charsLength) unquoted
        else {
          val c = chars(offset)
          c match {
            case '\\' =>
              // Escape sequence. Append next character.
              // If the escape was the last character, drop it.
              if (offset + 1 < charsLength) loop(offset + 2, unquoted + chars(offset + 1))
              else unquoted

            case '"' =>
              // We only expect the ending quote to be unescaped, in which case
              // we drop it (for the unquoted value done).
              // Otherwise keep any unescaped '"'.
              if (offset + 1 == charsLength) unquoted
              else loop(offset + 1, unquoted + '"')

            case _ =>
              // Append this character.
              loop(offset + 1, unquoted + c)
          }
        }
      }
      // Unquote and unescape characters.
      loop(1, "")
    } else {
      // Unquoted (atom) value.
      cleaned
    }
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
    }.filter(_ >= 0).getOrElse(-1L)
  }

  /**
   * Gets whether server handles bytes range.
   *
   * Server must return "Accept-Ranges: bytes" header.
   */
  def handleBytesRange(response: HttpResponse): Boolean = {
    Option(response.getFirstHeader(HttpHeaders.ACCEPT_RANGES)).map(_.getValue).contains("bytes")
  }

  /**
   * Gets response content range.
   *
   * Server must return a valid "Content-Range" header.
   */
  def getContentRange(response: HttpResponse): Option[ContentRange] = {
    Option(response.getFirstHeader(HttpHeaders.CONTENT_RANGE)).flatMap { h =>
      try {
        h.getValue match {
          case CONTENT_RANGE_REGEXP(start, end, total) =>
            Some(ContentRange(
              start = start.toLong,
              end = end.toLong,
              total = if (total == "*") Long.MaxValue else total.toLong
            ))

          case _ =>
            None
        }
      } catch {
        case ex: Exception =>
          logger.error(s"Failed to parse HTTP content range=<${h.getValue}>: ${ex.getMessage}", ex)
          None
      }
    }
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
