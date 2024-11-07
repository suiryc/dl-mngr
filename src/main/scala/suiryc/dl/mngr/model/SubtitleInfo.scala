package suiryc.dl.mngr.model

import com.typesafe.scalalogging.Logger
import suiryc.dl.mngr.util.Misc
import suiryc.scala.io.PathsEx

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}


/**
 * Subtitles info.
 *
 * Used both in live and backup info.
 */
case class SubtitleInfo(
  /** Subtitles raw content (if not already saved). */
  raw: Option[String],
  /** Subtitles file extension to use. */
  extension: String,
  /** Subtitles filename (if already saved). */
  filename: Option[String]
) {

  /**
   * Determines target path.
   *
   * Targets path is a sibling of the target download file: has the same file
   * name but subtitle extension.
   */
  def determinePath(download: Download): Path = {
    // Use download file name, and subtitle extension.
    val subtitleFilename = PathsEx.filename(PathsEx.atomicName(download.path), extension)
    // Subtitle file is saved next to target (not temporary)
    // download file.
    download.path.resolveSibling(subtitleFilename)
  }

  /**
   * Saves subtitle file.
   *
   * If not already done, saves raw subtitles content in file.
   * Updates download to remember saved filename, and forget raw content.
   */
  def prepare(logger: Logger, download: Download): Unit = {
    if (filename.isEmpty) {
      raw.foreach { raw =>
        val path = Misc.getAvailablePath(
          determinePath(download),
          dot = true
        )
        Files.write(path, raw.getBytes(StandardCharsets.UTF_8))
        val msg = s"Subtitle file=<${Misc.fileContext(path)}> saved"
        logger.info(s"${download.context} $msg")
        download.info.addLog(LogKind.Info, msg)

        download.setSubtitle(Some(copy(
          raw = None,
          filename = Some(path.getFileName.toString)
        )))
      }
    }
  }

}
