package suiryc.dl.mngr.model

import com.typesafe.scalalogging.Logger
import spray.json._
import suiryc.dl.mngr.util.HLSParser.TagValue
import suiryc.dl.mngr.util.{HLSParser, Http}
import suiryc.scala.spray.json.JsonFormats

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.Base64
import scala.io.Source


object HLSInfo extends DefaultJsonProtocol with JsonFormats {

  case class Key(raw: Option[String])

  implicit val hlsKeyFormat: RootJsonFormat[Key] = jsonFormat1(Key.apply)
  implicit val hlsInfoFormat: RootJsonFormat[HLSInfo] = jsonFormat3(HLSInfo.apply)

}

case class HLSInfo(
  /** HLS uri. */
  uri: URI,
  /** HLS raw content (if not already saved). */
  raw: Option[String],
  /** HLS keys (if not already saved). */
  keys: List[HLSInfo.Key]
) {

  /**
   * Prepares HLS processing.
   *
   * Creates temporary download path with:
   *  - keys if any
   *  - translated m3u8, referring to local (key/segment) files
   * Updates given download:
   *  - sets list of stream segments (link actual URIs to download and local
   *    files in translated m3u8)
   *  - drop raw HLS content (which has now been prepared
   */
  def prepare(logger: Logger, download: Download): Unit = {
    raw.foreach { raw =>
      prepare(logger, download, raw)
    }
  }

  private def prepare(logger: Logger, download: Download, raw: String): Unit = {
    if (!download.temporaryPath.toFile.exists()) {
      download.temporaryPath.toFile.mkdirs()
    }

    // Write keys in local files, and remember them.
    val keyFilenames = keys.zipWithIndex.map { case (key, keyIdx) =>
      key.raw.map { rawKey =>
        val filename = s"%04d.key".format(keyIdx)
        Files.write(
          download.temporaryPath.resolve(filename),
          Base64.getDecoder.decode(rawKey)
        )
        filename
      }
    }

    // Parse HLS stream playlist:
    //  - remember segment URIs, and replace them by local filenames
    //  - replace keys URIs by the local filenames we just created
    //  - leave other lines as-is
    val keysIt = keyFilenames.iterator
    val lines = Source.fromString(raw).getLines()
    @scala.annotation.tailrec
    def loop(m3u8: List[String], absolute: List[String], segments: List[StreamSegment]): (String, String, List[StreamSegment]) = {
      if (lines.hasNext) {
        val line = lines.next()
        HLSParser.parseTag(line) match {
          case Some(tag) =>
            tag.name match {
              case "EXTINF" =>
                // Next line is a segment URI. It may be relative to the HLS
                // URI.
                val segmentUri = uri.resolve(Http.getURI(lines.next()))
                val filename = "%05d.ts".format(segments.length)
                val segment = StreamSegment(segmentUri, filename)
                loop(
                  m3u8 ::: List(line, filename),
                  absolute ::: List(line, segment.uri.toString),
                  segments :+ segment
                )

              case "EXT-X-KEY" =>
                keysIt.next() match {
                  case Some(keyURI) =>
                    // Change this key URI.
                    val changed = tag.changeAttribute("URI", TagValue(keyURI, quoted = true)).toString
                    val changedAbsolute = tag.findAttribute("URI").map { attr =>
                      val keyURI = uri.resolve(Http.getURI(attr.value.value)).toString
                      tag.changeAttribute("URI", TagValue(keyURI, quoted = true)).toString
                    }.getOrElse(line)
                    loop(m3u8 :+ changed, absolute :+ changedAbsolute, segments)

                  case None =>
                    // No more prepare key: should not happen; just keep line
                    // as-is.
                    loop(m3u8 :+ line, absolute:+ line, segments)
                }

              case _ =>
                loop(m3u8 :+ line, absolute:+ line, segments)
            }

          case None =>
            loop(m3u8 :+ line, absolute:+ line, segments)
        }
      } else {
        (m3u8.mkString("\n"), absolute.mkString("\n"), segments)
      }
    }
    val (m3u8, absolute, segments) = loop(Nil, Nil, Nil)
    Files.write(
      download.temporaryPath.resolve("stream.m3u8"),
      m3u8.getBytes(StandardCharsets.UTF_8)
    )
    // Also save original and absolute stream info.
    Files.write(
      download.temporaryPath.resolve("original.m3u8"),
      raw.getBytes(StandardCharsets.UTF_8)
    )
    Files.write(
      download.temporaryPath.resolve("absolute.m3u8"),
      absolute.getBytes(StandardCharsets.UTF_8)
    )

    val msg = "HLS prepared"
    logger.info(s"${download.context} $msg")
    download.info.addLog(LogKind.Info, msg)

    download.info.streamSegments = segments
    download.setHLS(Some(copy(
      raw = None,
      keys = Nil
    )))
    ()
  }

}
