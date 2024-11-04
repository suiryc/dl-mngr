package suiryc.dl.mngr

import com.typesafe.scalalogging.StrictLogging
import javafx.stage.Stage
import monix.execution.Scheduler
import spray.json._
import suiryc.dl.mngr.I18N.Strings
import suiryc.dl.mngr.controllers.MainController
import suiryc.scala.akka.{AkkaResources, CoreSystem}
import suiryc.scala.io.SystemStreams
import suiryc.scala.javafx.{JFXApplication, JFXLauncher}
import suiryc.scala.log.{LoggerConfiguration, Loggers}
import suiryc.scala.misc.Util
import suiryc.scala.sys.UniqueInstance
import suiryc.scala.sys.UniqueInstance.CommandResult

import java.io.{Closeable, InputStream}
import java.nio.file.Path
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}
import scala.concurrent.{Await, Future, Promise}
import scala.io.StdIn
import scala.util.{Failure, Try}

object Main extends JFXLauncher[MainApp] with StrictLogging {

  // Note: use 'lazy' for fields that indirectly trigger stdout/stderr writing
  // (e.g. through logger, or scala Console println etc.), so that we can
  // redirect streams before it happens.

  import Akka.dispatcher

  val timeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")

  private val appPath: Path = Util.classLocation[this.type]

  private def appPathRelative(other: String): Path = appPath.resolve(other)

  val statePath: Path = appPathRelative("state.json")

  lazy val settings = new Settings(appPathRelative("application.conf"))

  val versionedName: String = s"${Info.name} ${Info.version}${
    Info.commitId.map(v => s" ($v)").getOrElse("")
  }"

  val buildTime: LocalDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(Info.buildTime), ZoneId.systemDefault)
  val buildTimeString: String = buildTime.format(timeFormatter)

  // Streams
  private val streams = SystemStreams()

  // Promise to complete when we are ready to process command arguments
  private[mngr] val promise = Promise[Unit]()

  // The running controller
  var controller: MainController = _

  private val parser: scopt.OptionParser[Params] = new scopt.OptionParser[Params](getClass.getCanonicalName) {
    def nonEmptyOrNone(s: String): Option[String] = Option(s).filterNot(_.trim.isEmpty)
    head(versionedName)
    help("help")
    opt[Unit]("auto").action { (_, c) =>
      c.copy(auto = Some(true))
    }
    opt[String]("comment").action { (v, c) =>
      c.copy(comment = nonEmptyOrNone(v))
    }
    opt[String]("cookie").action { (v, c) =>
      c.copy(cookie = nonEmptyOrNone(v))
    }
    opt[String]("file").action { (v, c) =>
      c.copy(file = nonEmptyOrNone(v))
    }
    opt[String]("http-referrer").action { (v, c) =>
      c.copy(referrer = nonEmptyOrNone(v))
    }
    opt[Boolean]("io-capture").action { (v, c) =>
      c.copy(ioCapture = Some(v))
    }
    opt[Unit]("json").action { (_, c) =>
      c.copy(json = Some(true))
    }
    opt[Long]("size").action { (v, c) =>
      c.copy(size = Some(v))
    }
    opt[String]("size-qualifier").action { (v, c) =>
      c.copy(sizeQualifier = Some(v))
    }
    opt[String]("unique-instance-id").action { (v, c) =>
      c.copy(uniqueInstanceId = Some(v))
    }
    opt[String]("url").action { (v, c) =>
      c.copy(url = nonEmptyOrNone(v))
    }
    opt[String]("user-agent").action { (v, c) =>
      c.copy(userAgent = nonEmptyOrNone(v))
    }
    opt[Unit]("version").foreach { _ =>
      println(
        s"""$versionedName
           |buildTime: $buildTimeString
           |scalaVersion: ${Info.scalaVersion}
           |sbtVersion: ${Info.sbtVersion}
           """.stripMargin)
      sys.exit(0)
    }
    opt[Unit]("ws").action { (_, c) =>
      c.copy(ws = Some(true))
    }
  }

  override def main(args: Array[String]): Unit = {
    // Notes:
    // Application.launch() creates a new instance of the enclosing class and
    // calls 'start' on it. Either we call it from a 'launch' method in the
    // target class, or pass the target class as parameter.
    // If values other than arguments need to be passed, the second approach is
    // to be used.
    LoggerConfiguration.setup()

    // Note: first parsing is to check arguments are ok (and we are good to
    // start the UI) and get unique instance appId.
    parser.parse(args, Params()) match {
      case Some(params) =>
        // Redirect stdout/stderr, so that potential command output result is
        // not mixed with garbage (logs, etc).
        // Note: scala 'Console' stores the current 'in/out/err' value. So
        // better not trigger it before redirecting streams. (methods to change
        // the values are marked deprecated)
        val ioCapture = params.ioCapture.contains(true) && Loggers.captureIo().nonEmpty
        val uniqueInstanceId = params.uniqueInstanceId.get
        val processed = UniqueInstance.start(uniqueInstanceId, _cmd, args, promise.future, streams)
        // We only end up here if we are the (first) unique instance.
        // Close initial streams when we are done processing the command: this
        // hints the caller process (e.g. our launcher script) that we are done
        // and that it can now process whatever we may have printed as result
        // of CLI processing.
        // Note: we do it if IO were captured, otherwise we assume caller does
        // not need it.
        if (ioCapture) {
          processed.onComplete { _ =>
            // Notes:
            // On Windows it may happen that some streams are 'fake' and cannot be
            // properly closed (invalid file descriptor exception). This is e.g.
            // the case for stdin when using 'javaw' (at least when spawned from
            // python without caring about its stdin).
            streams.out.flush()
            streams.err.flush()
            def close(l: String, c: Closeable): Unit = {
              try { c.close() }
              catch { case ex: Exception => println(s"Failed to close $l: $ex") }
            }
            close("stdin", streams.in)
            close("stdout", streams.out)
            close("stderr", streams.err)
          }
        }
        // 'launch' does not return until application is closed
        super.main(args)

      case None =>
        sys.exit(UniqueInstance.CODE_CMD_ERROR)
    }
  }

  private def _cmd(args: Array[String], input: InputStream): Future[CommandResult] = {
    // Replace stdin *and* scala Console.in
    val streams = SystemStreams.replace(input)
    try {
      Console.withIn(input) {
        // Second parsing to actually process the arguments through unique instance.
        parser.parse(args, Params()).map(parseJsonParams).map(cmd).getOrElse {
          // Should not happen
          Future.successful(CommandResult(UniqueInstance.CODE_CMD_ERROR, Some("Invalid arguments")))
        }
      }
    } finally {
      SystemStreams.restore(streams)
    }
  }

  private def parseJsonParams(params: Params): Params = {
    if (params.withJson) {
      val line = Option(StdIn.readLine()).map(_.trim).filterNot(_.isEmpty).getOrElse("{}")
      try {
        params.merge(line.parseJson.convertTo[Params])
      } catch {
        case ex: Exception =>
          logger.error(s"Invalid JSON input=<$line>", ex)
          params
      }
    } else {
      params
    }
  }

  def cmd(params: Params): Future[CommandResult] = {
    // Sanity check: we should not be able to process command arguments if we
    // failed to create the controller.
    if (controller != null) {
      val fWS = if (params.needWs) {
        WSServer.start()
      } else {
        Future.successful(-1)
      }
      val fExec = if (params.url.isDefined) {
        // Reminder: we are notified when the new download is taken into
        // account by the application, not whether it actually is added.
        // This is what we wish: it only matters that the application properly
        // started; any error will be displayed in the user interface and logs.
        controller.addDownload(params)
      } else {
        Future.successful(())
      }
      val f = for {
        wsPort <- fWS
        _ <- fExec
      } yield {
        val output = if (params.withJson) {
          val json = CommandOutput(
            wsPort = if (wsPort > 0) Some(wsPort) else None
          ).toJson.compactPrint
          Some(json)
        } else {
          if (wsPort > 0) Some(wsPort.toString) else None
        }
        CommandResult(UniqueInstance.CODE_SUCCESS, output)
      }
      f.andThen {
        case Failure(ex) =>
          // Display the issue in the GUI when possible.
          Option(controller).foreach(_.displayError(
            title = None,
            contentText = Some(Strings.cliIssue),
            ex = ex
          ))
        // At worst caller will log it.
      }
    } else {
      Future.successful(CommandResult(UniqueInstance.CODE_ERROR, Some("Controller is not started")))
    }
  }

  val Akka: AkkaResources = CoreSystem.NonBlocking

  lazy val scheduler: Scheduler = Akka.scheduler

  override def shutdown(stage: Stage): Unit = {
    // Reminder: we have to stop all JavaFX actors before closing stage and
    // stopping JavaFX thread. the easy way is to stop the system.
    val whenTerminated = WSServer.stop().flatMap { _ =>
      CoreSystem.terminate()
    }
    UniqueInstance.stop()
    Try(Await.ready(whenTerminated, Settings.SHUTDOWN_TIMEOUT)).toEither.swap.foreach { ex =>
      logger.error("Shutdown failed", ex)
    }
    LoggerConfiguration.stop()
    super.shutdown(stage)
  }

  case class Params(
    /** Whether to automatically process this new download. */
    auto: Option[Boolean] = None,
    /** Comment. */
    comment: Option[String] = None,
    /** Cookie. */
    cookie: Option[String] = None,
    /** WebSocket correlation id (for response). */
    correlationId: Option[String] = None,
    /** Filename. */
    file: Option[String] = None,
    /** Video HLS. */
    hls: Option[Params.HLS] = None,
    /** Whether to capture console I/O. */
    ioCapture: Option[Boolean] = Some(true),
    /** Whether params are passed as JSON (after command line). */
    json: Option[Boolean] = Some(false),
    /** Referrer URI. */
    referrer: Option[String] = None,
    /** Size hint (informational). */
    size: Option[Long] = None,
    /** Size qualifier (informational). */
    sizeQualifier: Option[String] = None,
    /** Video subtitles. */
    subtitle: Option[Params.Subtitle] = None,
    /** Unique instance id to use. */
    uniqueInstanceId: Option[String] = Some("suiryc.dl-mngr"),
    /** Remote URI to download from. */
    url: Option[String] = None,
    /** User agent. */
    userAgent: Option[String] = None,
    /** Whether to return (start if needed) WebSocket port. */
    ws: Option[Boolean] = None
  ) {

    def isAuto: Boolean = auto.contains(true)

    def withJson: Boolean = json.contains(true)

    def needWs: Boolean = ws.contains(true)

    def merge(other: Params): Params = {
      // Convert objects to JSON, then merge fields: 'other' overrides ours.
      // And convert back to Params.
      val fields = (this:Params).toJson.asJsObject.fields ++
        other.toJson.asJsObject.fields
      JsObject(fields).convertTo[Params]
    }

  }

  object Params extends DefaultJsonProtocol {

    case class HLSKey(
      method: String,
      url: Option[String],
      raw: Option[String]
    )

    case class HLS(
      raw: String,
      url: String,
      keys: List[HLSKey]
    )

    case class Subtitle(
      raw: String,
      url: String,
      filename: String,
      lang: Option[String],
      name: Option[String]
    )

    implicit val hlsKeyFormat: RootJsonFormat[HLSKey] = jsonFormat3(HLSKey)
    implicit val hlsFormat: RootJsonFormat[HLS] = jsonFormat3(HLS)
    implicit val subtitleFormat: RootJsonFormat[Subtitle] = jsonFormat5(Subtitle)
    implicit val paramsFormat: RootJsonFormat[Params] = jsonFormat16(Params.apply)

  }

  private case class CommandOutput(
    wsPort: Option[Int]
  )

  private object CommandOutput extends DefaultJsonProtocol {
    implicit val commandOutputFormat: RootJsonFormat[CommandOutput] = jsonFormat1(CommandOutput.apply)
  }

}

class MainApp extends JFXApplication {

  import Main._

  override def start(stage: Stage): Unit = {
    I18N.loadLocale()

    val state = MainController.State(
      stage = stage,
      dlMngr = new DownloadManager()
    )
    // First build the controller
    MainController.build(state)
    // Then complete the promise
    promise.trySuccess(())
    ()
  }

}
