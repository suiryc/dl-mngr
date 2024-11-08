package suiryc.dl.mngr.util

import com.typesafe.scalalogging.LazyLogging
import suiryc.scala.io.PathsEx

import java.nio.file.{Files, Path}


object Misc extends LazyLogging {

  private val DOT_NAMING = (n: Int) => s".$n"

  lazy val javaVersionInfo: String = {
    s"${System.getProperty("java.version")} (${System.getProperty("java.vm.name")})"
  }

  /** Returns filename and parent path if any. */
  def fileContext(path: Path): String = {
    // Keep the filename and parent path if any.
    val count = path.getNameCount
    if (count >= 3) path.subpath(count - 2, count).toString
    else path.toString
  }

  /**
   * Finds available path.
   *
   * Starting from given path, find the first name that is available, adding
   * " (n)" (or ".n") suffix.
   *
   * @param path path to test
   * @param isFile whether this is supposed to be a file path
   * @param dot whether to use dot (".n") or parenthesis (" (n)") for
   *            alternative suffix
   */
  def getAvailablePath(path: Path, isFile: Boolean = true, dot: Boolean = false): Path = {
    val alternative = if (dot) Some(DOT_NAMING) else None
    PathsEx.getAvailable(
      path,
      isFile = isFile,
      alternative = alternative
    )
  }

  /**
   * Creates directory.
   *
   * If path already exists, a new name will be automatically chosen.
   * When applicable, a suffix is inserted after name.
   *
   * @param path path to create
   * @return actual created path
   */
  def createDirectory(path: Path): Path = {
    @scala.annotation.tailrec
    def loop(remainingAttempts: Int): Path = {
      val probed = getAvailablePath(path, isFile = false)
      try {
        Files.createDirectory(probed)
      } catch {
        case ex: Exception =>
          if (remainingAttempts == 0) throw ex
          loop(remainingAttempts - 1)
      }
    }
    loop(3)
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
      val probed = getAvailablePath(target, dot = dot)
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

    if (target == source) {
      // Nothing to do: file is already target.
      target
    } else {
      // If we are competing to use the target name, try more than once.
      loop(3)
    }
  }

}
