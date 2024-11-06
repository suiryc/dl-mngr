package suiryc.dl.mngr.util

import com.typesafe.scalalogging.LazyLogging
import suiryc.scala.io.PathsEx

import java.nio.file.{Files, Path}


object Misc extends LazyLogging {

  /** Returns filename and parent path if any. */
  def fileContext(path: Path): String = {
    // Keep the filename and parent path if any.
    val count = path.getNameCount
    if (count >= 3) path.subpath(count - 2, count).toString
    else path.toString
  }

  /**
   * Moves (or renames if possible) file.
   *
   * If target already exists, a new name will be automatically chosen.
   * When applicable, a suffix is inserted after file name (before extension).
   *
   * @param source source path
   * @param target target path
   * @param dot whether to use dot (".n") or parenthesis (" (n)") for
   *            alternative suffix
   * @return actual target path
   */
  def moveFile(source: Path, target: Path, dot: Boolean = false): Path = {
    @scala.annotation.tailrec
    def loop(remainingAttempts: Int): Path = {
      val alternative = if (dot) {
        Some((n: Int) => s".$n")
      } else {
        None
      }
      val probed = PathsEx.getAvailable(target, alternative = alternative)
      if (probed != target) logger.warn(s"Path=<$target> already exists; saving to=<$probed> instead")
      try {
        Files.move(source, probed)
        probed
      } catch {
        case ex: Exception =>
          if (remainingAttempts == 0) throw ex
          loop(remainingAttempts - 1)
      }
    }

    // If we are competing to use the target name, try more than once.
    loop(3)
  }

}
