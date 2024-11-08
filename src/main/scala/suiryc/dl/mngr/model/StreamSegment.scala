package suiryc.dl.mngr.model

import spray.json._
import suiryc.scala.spray.json.JsonFormats

import java.net.URI
import java.nio.file.Path


object StreamSegment extends DefaultJsonProtocol with JsonFormats {

  implicit val streamSegmentFormat: RootJsonFormat[StreamSegment] = jsonFormat2(StreamSegment.apply)

}

case class StreamSegment(
  /** Segment uri. */
  uri: URI,
  /** Local filename, where to save segment file. */
  filename: String
) {

  /**
   * Determines target path.
   *
   * Targets path is a child of the temporary download path folder.
   */
  def determinePath(download: Download): Path = {
    download.temporaryPath.resolve(filename)
  }

}
