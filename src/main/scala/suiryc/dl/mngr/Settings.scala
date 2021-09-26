package suiryc.dl.mngr

import com.typesafe.scalalogging.StrictLogging
import suiryc.dl.mngr.model.LogKind
import suiryc.scala.settings.{BaseConfig, ConfigEntry, PortableSettings}
import suiryc.scala.io.RichFile
import suiryc.scala.sys.{Command, OS}

import java.net.URI
import java.nio.file.{Path, Paths}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

object Settings {

  val DL_STOP_TIMEOUT: FiniteDuration = 15.seconds
  val SHUTDOWN_TIMEOUT: FiniteDuration = 10.seconds

  private[mngr] val KEY_SUIRYC = "suiryc"
  private[mngr] val KEY_DL_MNGR = "dl-mngr"

  private[mngr] val KEY_LOCATION = "location"
  private[mngr] val KEY_STAGE = "stage"

  private val KEY_ASK = "ask"
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
  private val KEY_ENABLED = "enabled"
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
  private val KEY_PATTERN = "pattern"
  private val KEY_PREALLOCATE = "preallocate"
  private val KEY_PROXY = "proxy"
  private val KEY_RATE_LIMIT = "rate-limit"
  private val KEY_READ = "read"
  private val KEY_REMOVE_COMPLETED = "remove-completed"
  private val KEY_SEGMENTS = "segments"
  private val KEY_SERVER_MAX = "server-max"
  private val KEY_SITES = "sites"
  private val KEY_SIZE = "size"
  private val KEY_SOCKET = "socket"
  private val KEY_SSL = "ssl"
  private val KEY_TIMEOUT = "timeout"
  private val KEY_TRUST = "trust"
  private val KEY_VALUE = "value"
  private val KEY_UNIT = "unit"
  private val KEY_URL = "url"
  private val KEY_WRITE = "write"
  private val KEY_ZERO = "zero"

  private[mngr] val prefix = List(KEY_SUIRYC, KEY_DL_MNGR)
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

class Settings(path: Path) extends StrictLogging {

  import Settings._

  private[mngr] val settings = PortableSettings(path, prefix)

  // Note: we are sure (reference.conf) that there is at least the 'default'
  // 'sites' entry.
  val sitesDefault: SiteSettings = new SiteSettings(KEY_DEFAULT)
  private var sites: Map[String, SiteSettings] = {
    settings.config.getConfig(BaseConfig.joinPath(sitesPrefix)).root.keySet.asScala.map { key =>
      key.toLowerCase
    }.filterNot { key =>
      key == KEY_DEFAULT
    }.map { key =>
      key -> new SiteSettings(key)
    }.toMap
  }

  private var sitePatterns: Map[Regex, SiteSettings] = {
    sites.values.flatMap { site =>
      site.regex.map { pattern =>
        pattern -> site
      }
    }.toMap
  }

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

  val proxyEnabled: ConfigEntry[Boolean] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_PROXY, KEY_ENABLED))
  val proxy: ConfigEntry[String] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_DOWNLOADS, KEY_PROXY, KEY_URL))

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

  val preallocateEnabled: ConfigEntry[Boolean] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_FILESYSTEM, KEY_PREALLOCATE, KEY_ENABLED))
  val preallocateZero: ConfigEntry[Boolean] =
    ConfigEntry.from(settings, prefix ++ Seq(KEY_FILESYSTEM, KEY_PREALLOCATE, KEY_ZERO))

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

  def findSite(uri: URI): SiteSettings = {
    @scala.annotation.tailrec
    def loop(host: String, full: Boolean): SiteSettings = {
      // Site name must not be TLD alone
      val els = host.split("\\.", 2)
      if (els.size > 1) {
        // Check whether this host is known, otherwise go up one level.
        // As a fallback, we also check full host name against site patterns.
        lazy val byPattern =
          if (full) sitePatterns.find(_._1.findFirstIn(host).nonEmpty).map(_._2)
          else None
        sites.get(host).orElse(byPattern) match {
          case Some(siteSettings) => siteSettings
          case None => loop(els(1), full = false)
        }
      } else {
        // Fallback to default
        sitesDefault
      }
    }
    loop(uri.getHost.toLowerCase, full = true)
  }

  def findServerSite(host: String): SiteSettings = {
    try {
      findSite(new URI("http", host, null, null))
    } catch {
      case _: Exception => sitesDefault
    }
  }

  def getSite(site0: String): SiteSettings = {
    val site = site0.toLowerCase
    // Callers should only request known sites. If not, fallback to default site
    // and log this bad access (with stacktrace to pinpoint caller).
    val s = sites.getOrElse(site, sitesDefault)
    if (s.isDefault && (site != KEY_DEFAULT)) {
      val msg = s"Using default site instead of missing requested site=<$site>"
      val ex = new Exception("Unknown site")
      logger.warn(msg, ex)
      Main.controller.addLog(LogKind.Warning, msg, Some(ex))
    }
    s
  }

  def obtainSite(site0: String): SiteSettings = {
    val site = site0.toLowerCase
    if (site == KEY_DEFAULT) throw new Exception("Cannot access default site settings this way")
    sites.getOrElse(site, {
      val s = new SiteSettings(site)
      sites += (site -> s)
      s
    })
  }

  def changeSitePattern(site: SiteSettings, pattern: Option[Regex]): Unit = {
    site.regex.foreach(sitePatterns -= _)
    pattern.foreach { p =>
      sitePatterns += (p -> site)
    }
  }

  def removeSite(s: SiteSettings): Unit = {
    val site = s.site
    if (site == KEY_DEFAULT) throw new Exception("Cannot access default site settings this way")
    s.remove()
    sites -= site
    changeSitePattern(s, None)
  }

  def removeSite(site0: String): Unit = {
    val site = site0.toLowerCase
    if (site == KEY_DEFAULT) throw new Exception("Cannot access default site settings this way")
    sites.get(site).foreach(removeSite)
  }

  def getTemporaryFile(path: Path): Option[Path] = {
    downloadsExtension.opt.filterNot(_.trim.isEmpty).map { extension =>
      path.resolveSibling(s"${path.getFileName.toString}.$extension")
    }
  }

  class SiteSettings(val site: String) {

    import Settings._

    val isDefault: Boolean = site == KEY_DEFAULT

    private val settingsPrefix = sitesPrefix :+ site

    val pattern: ConfigEntry[String] =
      ConfigEntry.from(settings, settingsPrefix ++ Seq(KEY_PATTERN))
    val sslTrust: ConfigEntry[Boolean] =
      ConfigEntry.from(settings, settingsPrefix ++ Seq(KEY_SSL, KEY_TRUST))
    val sslErrorAsk: ConfigEntry[Boolean] =
      ConfigEntry.from(settings, settingsPrefix ++ Seq(KEY_SSL, KEY_ERROR, KEY_ASK))
    val cnxMax: ConfigEntry[Int] =
      ConfigEntry.from(settings, settingsPrefix ++ Seq(KEY_CONNECTION, KEY_MAX))
    val segmentsMax: ConfigEntry[Int] =
      ConfigEntry.from(settings, settingsPrefix ++ Seq(KEY_SEGMENTS, KEY_MAX))

    private def getPattern: Option[Regex] = pattern.opt.filterNot(_.isBlank).map(_.r)
    var regex: Option[Regex] = getPattern

    def refreshPattern(): Unit = {
      val p = getPattern
      changeSitePattern(this, p)
      regex = p
    }

    def setPattern(s: Option[String]): Unit = {
      val actual = s.getOrElse("")
      if (actual.isBlank) pattern.reset()
      else pattern.set(actual)
      refreshPattern()
    }

    def getSslTrust: Boolean = sslTrust.opt.getOrElse {
      Settings.this.sitesDefault.sslTrust.get
    }

    def getCnxMax: Int = cnxMax.opt.orElse {
      Settings.this.sitesDefault.cnxMax.opt
    }.getOrElse {
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
