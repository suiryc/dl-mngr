package suiryc.dl.mngr.model

import spray.json._
import suiryc.scala.spray.json.JsonFormats

object JsonImplicits extends DefaultJsonProtocol with JsonFormats {

  implicit val segmentRangeFormat: RootJsonFormat[SegmentRange] = jsonFormat2(SegmentRange.apply)
  implicit val downloadBackupFormat: RootJsonFormat[DownloadBackupInfo] = jsonFormat16(DownloadBackupInfo)

}
