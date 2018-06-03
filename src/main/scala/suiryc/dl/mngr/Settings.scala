package suiryc.dl.mngr

import java.nio.file.{Path, Paths}
import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration
import suiryc.scala.settings.{BaseConfig, ConfigEntry, PortableSettings}
import suiryc.scala.io.RichFile
import suiryc.scala.sys.{Command, OS}

object Settings {

  private[mngr] val KEY_SUIRYC = "suiryc"
  private[mngr] val KEY_DL_MNGR = "dl-mngr"

  private[mngr] val KEY_STAGE = "stage"

  private val KEY_AUTO_SAVE = "auto-save"
  private val KEY_BUFFER = "buffer"
  private val KEY_CODE = "code"
  private val KEY_CONNECT = "connect"
  private val KEY_CONNECTION = "connection"
  private val KEY_CONNECTION_REQUEST = "connection-request"
  private val KEY_DEBUG = "debug"
  private val KEY_DEFAULT = "default"
  private val KEY_DELAY = "delay"
  private val KEY_DOWNLOADS = "downloads"
  private val KEY_ERROR = "error"
  private val KEY_EXTENSION = "extension"
  private val KEY_FILESYSTEM = "filesystem"
  private val KEY_FLUSH = "flush"
  private val KEY_IDLE = "idle"
  private val KEY_LOCALE = "locale"
  private val KEY_MAX = "max"
  private val KEY_MIN = "min"
  private val KEY_MIN_SIZE = "min-size"
  private val KEY_PATH = "path"
  private val KEY_PROXY = "proxy"
  private val KEY_RATE_LIMIT = "rate-limit"
  private val KEY_READ = "read"
  private val KEY_REMOVE_COMPLETED = "remove-completed"
  private val KEY_SEGMENTS = "segments"
  private val KEY_SERVER_MAX = "server-max"
  private val KEY_SITES = "sites"
  private val KEY_SIZE = "size"
  private val KEY_SOCKET = "socket"
  private val KEY_TIMEOUT = "timeout"
  private val KEY_VALUE = "value"
  private val KEY_UNIT = "unit"
  private val KEY_WRITE = "write"

  private val prefix = List(KEY_SUIRYC, KEY_DL_MNGR)
  private val sitesPrefix = prefix :+ KEY_SITES

  val defaultDownloadPath: Path = {
    val downloadFolder = RichFile.userHome.toPath.resolve("Downloads")
    val downloadPath = if (OS.isLinux) {
      val r = Command.execute(Seq("xdg-user-dir", "DOWNLOAD"))
      if (r.exitCode == 0) Paths.get(r.stdout)
      else downloadFolder
    } else {
      downloadFolder
    }
    @scala.annotation.tailrec
    def loop(path: Path): Path = {
      if (path.toFile.exists) path
      else loop(path.getParent)
    }

    loop(downloadPath)
  }

}

class Settings(path: Path) {

  import Settings._

  private[mngr] val settings = PortableSettings(path, prefix)

  private var sites: Map[String, SiteSettings] = {
    val path = BaseConfig.joinPath(sitesPrefix)
    if (settings.config.hasPath(path)) {
      settings.config.getConfig(path).root.keySet.asScala.map { key0 ⇒
        val key = key0.toLowerCase
        key → new SiteSettings(key)
      }.toMap
    } else Map.empty
  }

  val sitesDefault: SiteSettings = sites.getOrElse(KEY_DEFAULT, new SiteSettings(KEY_DEFAULT))
  sites -= KEY_DEFAULT

  val localeCode: ConfigEntry[String] =
    ConfigEntry.from[String](settings, prefix ++ Seq(KEY_LOCALE, KEY_CODE))

  val debug: ConfigEntry[Boolean] =
    ConfigEntry.from[Boolean](settings, prefix ++ Seq(KEY_DEBUG))

  val downloadsPath: ConfigEntry[Path] =
    ConfigEntry.from[Path](settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_PATH))
      .withDefault(defaultDownloadPath)
  val downloadsExtension: ConfigEntry[String] =
    ConfigEntry.from[String](settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_EXTENSION))

  val autosaveDelay: ConfigEntry[FiniteDuration] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_AUTO_SAVE))
  val removeCompleted: ConfigEntry[Boolean] =
    ConfigEntry.from[Boolean](settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_REMOVE_COMPLETED))

  val rateLimitValue: ConfigEntry[Long] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_RATE_LIMIT, KEY_VALUE))
  val rateLimitUnit: ConfigEntry[String] =
    ConfigEntry.from[String](settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_RATE_LIMIT, KEY_UNIT))

  val proxy: ConfigEntry[String] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_PROXY))

  val connectionRequestTimeout: ConfigEntry[FiniteDuration] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_TIMEOUT, KEY_CONNECTION_REQUEST))
  val connectTimeout: ConfigEntry[FiniteDuration] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_TIMEOUT, KEY_CONNECT))
  val socketTimeout: ConfigEntry[FiniteDuration] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_TIMEOUT, KEY_SOCKET))
  val idleTimeout: ConfigEntry[FiniteDuration] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_TIMEOUT, KEY_IDLE))

  val errorMax: ConfigEntry[Int] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_ERROR, KEY_MAX))
  val errorDelay: ConfigEntry[FiniteDuration] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_ERROR, KEY_DELAY))

  val bufferReadMin: ConfigEntry[Long] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_BUFFER, KEY_READ, KEY_MIN))(ConfigEntry.bytesHandler)
  val bufferReadMax: ConfigEntry[Long] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_BUFFER, KEY_READ, KEY_MAX))(ConfigEntry.bytesHandler)

  val bufferWriteFlushSize: ConfigEntry[Long] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_FILESYSTEM, KEY_BUFFER, KEY_WRITE, KEY_FLUSH, KEY_SIZE))(ConfigEntry.bytesHandler)

  val segmentsMinSize: ConfigEntry[Long] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_SEGMENTS, KEY_MIN_SIZE))(ConfigEntry.bytesHandler)

  val cnxMax: ConfigEntry[Int] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_CONNECTION, KEY_MAX))
  val cnxServerMax: ConfigEntry[Int] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_CONNECTION, KEY_SERVER_MAX))
  val downloadsMax: ConfigEntry[Int] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_MAX))

  def getSites: Map[String, SiteSettings] = sites

  def getSite(site0: String): SiteSettings = {
    val site = site0.toLowerCase
    if (site == KEY_DEFAULT) throw new Exception("Cannot access default site settings this way")
    sites.getOrElse(site, {
      val s = new SiteSettings(site)
      sites += (site → s)
      s
    })
  }

  def removeSite(s: SiteSettings): Unit = {
    val site = s.site
    if (site == KEY_DEFAULT) throw new Exception("Cannot access default site settings this way")
    s.remove()
    sites -= site
  }

  def removeSite(site0: String): Unit = {
    val site = site0.toLowerCase
    if (site == KEY_DEFAULT) throw new Exception("Cannot access default site settings this way")
    sites.get(site).foreach(removeSite)
  }

  def getTemporaryFile(path: Path): Option[Path] = {
    downloadsExtension.opt.filterNot(_.trim.isEmpty).map { extension ⇒
      path.resolveSibling(s"${path.getFileName.toString}.$extension")
    }
  }

  class SiteSettings(val site: String) {

    import Settings._

    val isDefault: Boolean = site == KEY_DEFAULT

    private val settingsPrefix = sitesPrefix :+ site

    val cnxMax: ConfigEntry[Int] =
      ConfigEntry.from(settings, settingsPrefix ++ Seq(KEY_CONNECTION, KEY_MAX))
    val segmentsMax: ConfigEntry[Int] =
      ConfigEntry.from(settings, settingsPrefix ++ Seq(KEY_SEGMENTS, KEY_MAX))

    def getCnxMax: Int = cnxMax.opt.orElse{
      Settings.this.sitesDefault.cnxMax.opt
    }.getOrElse{
      Settings.this.cnxMax.get
    }

    def getSegmentsMax: Int = segmentsMax.opt.getOrElse {
      Settings.this.sitesDefault.segmentsMax.get
    }

    def remove(): Unit = {
      settings.withoutPath(BaseConfig.joinPath(settingsPrefix))
    }

  }

}
