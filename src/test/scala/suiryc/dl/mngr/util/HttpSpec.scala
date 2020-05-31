package suiryc.dl.mngr.util

import java.net.URLEncoder
import org.apache.http.entity.{ContentType, StringEntity}
import org.apache.http._
import org.apache.http.message.{BasicHeader, BasicHeaderValueFormatter, BasicHttpResponse, BasicNameValuePair}
import org.apache.http.util.CharArrayBuffer
import org.scalatest.{Matchers, WordSpec}
import suiryc.dl.mngr.model.SegmentRange
import suiryc.dl.mngr.util.Http.ContentRange

class HttpSpec extends WordSpec with Matchers {

  "Http" when {

    "getting content length from HTTP response" should {

      "handle valid value" in {
        Http.getContentLength(buildHttpResponse(contentLength = Some("1234"))) shouldBe 1234L
      }

      "handle missing value" in {
        Http.getContentLength(buildHttpResponse()) shouldBe -1L
      }

      "handle invalid value" in {
        Http.getContentLength(buildHttpResponse(contentLength = Some("-1234"))) shouldBe -1L
        Http.getContentLength(buildHttpResponse(contentLength = Some("1.234"))) shouldBe -1L
        Http.getContentLength(buildHttpResponse(contentLength = Some("invalid"))) shouldBe -1L
      }

    }

    "getting content range from HTTP response" should {

      "handle valid value" in {
        Http.getContentRange(buildHttpResponse(contentRange = Some("bytes 12-34/56"))) shouldBe Some(ContentRange(12, 34, 56))
        Http.getContentRange(buildHttpResponse(contentRange = Some("bytes 12-34/*"))) shouldBe Some(ContentRange(12, 34, Long.MaxValue))
        // Also handle extra white characters
        Http.getContentRange(buildHttpResponse(contentRange = Some(" \tbytes \t12-34/56 \t"))) shouldBe Some(ContentRange(12, 34, 56))
      }

      "handle missing value" in {
        Http.getContentRange(buildHttpResponse()) shouldBe None
      }

      "handle invalid value" in {
        Http.getContentRange(buildHttpResponse(contentRange = Some("something 12-34/56"))) shouldBe None
        Http.getContentRange(buildHttpResponse(contentRange = Some("bytes -34/56"))) shouldBe None
        Http.getContentRange(buildHttpResponse(contentRange = Some("bytes 12-/56"))) shouldBe None
        Http.getContentRange(buildHttpResponse(contentRange = Some("bytes 34/56"))) shouldBe None
        Http.getContentRange(buildHttpResponse(contentRange = Some("bytes 12-34/"))) shouldBe None
        Http.getContentRange(buildHttpResponse(contentRange = Some("bytes a-34/56"))) shouldBe None
        Http.getContentRange(buildHttpResponse(contentRange = Some("bytes 12-a/56"))) shouldBe None
        Http.getContentRange(buildHttpResponse(contentRange = Some("bytes 12-34/a"))) shouldBe None
        Http.getContentRange(buildHttpResponse(contentRange = Some("bytes 12-34/**"))) shouldBe None
        Http.getContentRange(buildHttpResponse(contentRange = Some("bytes 12-34/*56"))) shouldBe None
        Http.getContentRange(buildHttpResponse(contentRange = Some("bytes 12-34/56*"))) shouldBe None
      }

    }

    "matching ContentRange and SegmentRange" should {

      "match same range" in {
        ContentRange(12, 34, 56).matches(SegmentRange(12, 34)) shouldBe true
        ContentRange(12, 34, 56).matches(SegmentRange(12)) shouldBe true
      }

      "not match different range" in {
        ContentRange(12, 34, 56).matches(SegmentRange(12, 35)) shouldBe false
        ContentRange(12, 34, 56).matches(SegmentRange(11)) shouldBe false
      }

    }

    "decoding HTTP header parameter string" should {

      "handle plain atom value" in {
        Http.decodeString("atom", rfc8187 = false) shouldBe "atom"
        Http.decodeString(" value ", rfc8187 = false) shouldBe "value"
        Http.decodeString(" value 2 ", rfc8187 = false) shouldBe "value 2"
      }

      "handle quoted value" in {
        Http.decodeString(""""quoted"""", rfc8187 = false) shouldBe """quoted"""
        Http.decodeString(""""quoted value"""", rfc8187 = false) shouldBe """quoted value"""
        Http.decodeString(""" "quoted" """, rfc8187 = false) shouldBe """quoted"""
        Http.decodeString(""""qu\\o\"te\nd"""", rfc8187 = false) shouldBe """qu\o"tend"""
      }

      "handle unproperly quoted value" in {
        // Test:
        //  - unescaped quote
        //  - missing ending quote, preceded by escaped quote
        Http.decodeString(""" "quoted "value\"""", rfc8187 = false) shouldBe """quoted "value""""
      }

      "handle RFC 8187 encoded value" in {
        // Lowercase encoding, present language tag, uppercase percent-encoding.
        Http.decodeString("utf-8'en'%C2%A3%20rates", rfc8187 = true) shouldBe "£ rates"
        // Uppercase encoding, missing language tag, lowercase percent-encoding.
        Http.decodeString("UTF-8''%c2%a3%20and%20%e2%82%ac%20rates", rfc8187 = true) shouldBe "£ and € rates"
      }

      "handle invalid RFC 8187 format" in {
        // No charset nor language tag
        Http.decodeString(" \"test ", rfc8187 = true) shouldBe "\"test"
        // Either no charset or no language tag
        Http.decodeString("utf-8' test", rfc8187 = true) shouldBe "utf-8' test"
      }

      "handle invalid RFC 8187 charset encoding" in {
        Http.decodeString("utf-8'en'some value", rfc8187 = true) shouldBe "some value"
        Http.decodeString("no valid charset here'en'some value", rfc8187 = true) shouldBe "no valid charset here'en'some value"
      }

    }

    "getting filename from HTTP response" should {

      "return nothing if there is no matching parameter" in {
        Http.getFilename(buildHttpResponse()) shouldBe empty
        Http.getFilename(buildHttpResponse(ctype = true)) shouldBe empty
        Http.getFilename(buildHttpResponse(cdisp = true)) shouldBe empty
        Http.getFilename(buildHttpResponse(ctype = true, cdisp = true)) shouldBe empty
      }

      "return simple Content-Type 'name' when available" in {
        val response = buildHttpResponse(
          ctype = true,
          ctypeName = Some("plain name")
        )
        Http.getFilename(response) shouldBe Some("plain name")
      }

      "prefer encoded Content-Type 'name' when available" in {
        val response = buildHttpResponse(
          ctype = true,
          ctypeName = Some("plain name"),
          ctypeEncoded = Some("encoded name")
        )
        Http.getFilename(response) shouldBe Some("encoded name")
      }

      "prefer simple Content-Disposition 'filename' when available" in {
        val response = buildHttpResponse(
          ctype = true,
          ctypeName = Some("plain name"),
          ctypeEncoded = Some("encoded name"),
          cdisp = true,
          cdispName = Some("plain filename")
        )
        Http.getFilename(response) shouldBe Some("plain filename")
      }

      "prefer encoded Content-Disposition 'filename' when available" in {
        val response = buildHttpResponse(
          ctype = true,
          ctypeName = Some("plain name"),
          ctypeEncoded = Some("encoded name"),
          cdisp = true,
          cdispName = Some("plain filename"),
          cdispEncoded = Some("encoded filename")
        )
        Http.getFilename(response) shouldBe Some("encoded filename")
      }

    }

  }

  private def buildHttpResponse(contentLength: Option[String] = None, contentRange: Option[String] = None,
    ctype: Boolean = false, ctypeName: Option[String] = None, ctypeEncoded: Option[String] = None,
    cdisp: Boolean = false, cdispName: Option[String] = None, cdispEncoded: Option[String] = None): HttpResponse =
  {
    val response = new BasicHttpResponse(HttpVersion.HTTP_1_1, HttpStatus.SC_OK, "Ok")

    contentLength.foreach { value =>
      response.addHeader(new BasicHeader(HttpHeaders.CONTENT_LENGTH, value))
    }
    contentRange.foreach { value =>
      response.addHeader(new BasicHeader(HttpHeaders.CONTENT_RANGE, value))
    }

    if (ctype) {
      val parameters = List(
        new BasicNameValuePair("key", "value")
      ) ::: ctypeEncoded.map {value =>
        new BasicNameValuePair("name*", encodeRFC8187(value))
      }.toList ::: ctypeName.map { value =>
        new BasicNameValuePair("name", value)
      }.toList
      val contentType = ContentType.TEXT_PLAIN.withParameters(parameters: _*)
      response.setEntity(new StringEntity("", contentType))
      response.addHeader(new BasicHeader(HttpHeaders.CONTENT_TYPE, contentType.toString))
    }

    if (cdisp) {
      val parameters = List(
        new BasicNameValuePair("key", "value")
      ) ::: cdispEncoded.map {value =>
        new BasicNameValuePair("filename*", encodeRFC8187(value))
      }.toList ::: cdispName.map { value =>
        new BasicNameValuePair("filename", value)
      }.toList
      val buf = new CharArrayBuffer(16)
      buf.append("attachment")
      if (parameters.nonEmpty) buf.append("; ")
      BasicHeaderValueFormatter.INSTANCE.formatParameters(buf, parameters.toArray, false)
      response.addHeader(new BasicHeader(Http.HEADER_CONTENT_DISPOSITION, buf.toString))
    }

    response
  }

  private def encodeRFC8187(value: String): String = s"utf-8''${URLEncoder.encode(value, "UTF-8")}"

}
