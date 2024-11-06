package suiryc.dl.mngr.model

import suiryc.scala.io.PathsEx

import java.nio.file.Path


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

}
