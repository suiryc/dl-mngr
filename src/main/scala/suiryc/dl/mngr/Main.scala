package suiryc.dl.mngr

import akka.actor.ActorSystem
import java.io.Closeable
import java.nio.file.Path
import javafx.stage.Stage
import monix.execution.Scheduler
import scala.concurrent.{ExecutionContextExecutor, Future, Promise}
import scala.util.Failure
import suiryc.dl.mngr.I18N.Strings
import suiryc.dl.mngr.controllers.MainController
import suiryc.dl.mngr.model.NewDownloadInfo
import suiryc.scala.akka.CoreSystem
import suiryc.scala.io.SystemStreams
import suiryc.scala.javafx.{JFXApplication, JFXLauncher}
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.log.Loggers
import suiryc.scala.misc.Util
import suiryc.scala.sys.UniqueInstance
import suiryc.scala.sys.UniqueInstance.CommandResult

object Main extends JFXLauncher[MainApp] {

  // Note: use 'lazy' for fields that indirectly trigger stdout/stderr writing
  // (e.g. through logger, or scala Console println etc), so that we can
  // redirect streams before it happens.

  import Akka.dispatcher

  val appPath: Path = Util.classLocation[this.type]

  def appPathRelative(other: String): Path = appPath.resolve(other)

  val statePath: Path = appPathRelative("state.json")

  lazy val settings = new Settings(appPathRelative("application.conf"))

  val versionedName: String = s"${suiryc.dl.mngr.Info.name} ${suiryc.dl.mngr.Info.version}" +
    suiryc.dl.mngr.Info.gitHeadCommit.map(v ⇒ s" ($v)").getOrElse("")

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
    opt[Unit]("auto").action { (_, c) ⇒
      c.copy(auto = Some(true))
    }
    opt[String]("comment").action { (v, c) ⇒
      c.copy(comment = nonEmptyOrNone(v))
    }
    opt[String]("cookie").action { (v, c) ⇒
      c.copy(cookie = nonEmptyOrNone(v))
    }
    opt[String]("file").action { (v, c) ⇒
      c.copy(file = nonEmptyOrNone(v))
    }
    opt[String]("http-referrer").action { (v, c) ⇒
      c.copy(referrer = nonEmptyOrNone(v))
    }
    opt[Boolean]("io-capture").action { (v, c) ⇒
      c.copy(ioCapture = Some(v))
    }
    opt[Long]("size").action { (v, c) ⇒
      c.copy(size = Some(v))
    }
    opt[String]("unique-instance-id").action { (v, c) ⇒
      c.copy(uniqueInstanceId = Some(v))
    }
    opt[String]("url").action { (v, c) ⇒
      c.copy(url = nonEmptyOrNone(v))
    }
    opt[String]("user-agent").action { (v, c) ⇒
      c.copy(userAgent = nonEmptyOrNone(v))
    }
    opt[Unit]("version").foreach { _ ⇒
      println(
        s"""$versionedName
           |scalaVersion: ${Info.scalaVersion}
           |sbtVersion: ${Info.sbtVersion}
           """.stripMargin)
      sys.exit(0)
    }
    opt[Unit]("ws").action { (_, c) ⇒
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

    // Note: first parsing is to check arguments are ok (and we are good to
    // start the UI) and get unique instance appId.
    parser.parse(args, Params()) match {
      case Some(params) ⇒
        // Redirect stdout/stderr, so that potential command output result is
        // not mixed with garbage (logs, etc).
        // Note: scala 'Console' stores the current 'in/out/err' value. So
        // better not trigger it before redirecting streams. (methods to change
        // the values are marked deprecated)
        val ioCapture = params.ioCapture.contains(true) && Loggers.captureIo().nonEmpty
        val uniqueInstanceId = params.uniqueInstanceId.get
        val processed = UniqueInstance.start(uniqueInstanceId, _cmd _, args, promise.future, streams)
        // We only end up here if we are the (first) unique instance.
        // Close initial streams when we are done processing the command: this
        // hints the caller process (e.g. our launcher script) that we are done
        // and that it can now process whatever we may have printed as result
        // of CLI processing.
        // Note: we do it if IO were captured, otherwise we assume caller does
        // not need it.
        if (ioCapture) {
          processed.onComplete { _ ⇒
            // Notes:
            // On Windows it may happen that some streams are 'fake' and cannot be
            // properly closed (invalid file descriptor exception). This is e.g.
            // the case for stdin when using 'javaw' (at least when spawned from
            // python without caring about its stdin).
            streams.out.flush()
            streams.err.flush()
            def close(l: String, c: Closeable): Unit = {
              try { c.close() }
              catch { case ex: Exception ⇒ println(s"Failed to close $l: $ex") }
            }
            close("stdin", streams.in)
            close("stdout", streams.out)
            close("stderr", streams.err)
          }
        }
        // 'launch' does not return until application is closed
        super.main(args)

      case None ⇒
        sys.exit(UniqueInstance.CODE_CMD_ERROR)
    }
  }

  protected def _cmd(args: Array[String]): Future[CommandResult] = {
    // Second parsing to actually process the arguments through unique instance.
    parser.parse(args, Params()).map(cmd).getOrElse {
      // Should not happen
      Future.successful(CommandResult(UniqueInstance.CODE_CMD_ERROR, Some("Invalid arguments")))
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
        val dlInfo = NewDownloadInfo(
          auto = params.isAuto,
          uri = params.url,
          referrer = params.referrer,
          cookie = params.cookie,
          userAgent = params.userAgent,
          file = params.file,
          sizeHint = params.size,
          comment = params.comment
        )
        controller.addDownload(dlInfo)
      } else {
        Future.successful(())
      }
      val f = for {
        wsPort ← fWS
        _ ← fExec
      } yield {
        CommandResult(UniqueInstance.CODE_SUCCESS, if (wsPort > 0) Some(wsPort.toString) else None)
      }
      f.andThen {
        case Failure(ex) ⇒
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

  object Akka {

    implicit val system: ActorSystem = CoreSystem.system
    implicit val dispatcher: ExecutionContextExecutor = system.dispatcher

  }

  lazy val scheduler: Scheduler = CoreSystem.scheduler

  override def shutdown(): Unit = {
    WSServer.stop()
    UniqueInstance.stop()
    // Note: we share the same system
    JFXSystem.terminate()
    ()
  }

  case class Params(
    auto: Option[Boolean] = None,
    comment: Option[String] = None,
    cookie: Option[String] = None,
    correlationId: Option[String] = None,
    file: Option[String] = None,
    ioCapture: Option[Boolean] = Some(true),
    referrer: Option[String] = None,
    size: Option[Long] = None,
    uniqueInstanceId: Option[String] = Some("suiryc.dl-mngr"),
    url: Option[String] = None,
    userAgent: Option[String] = None,
    ws: Option[Boolean] = None
  ) {
    def isAuto: Boolean = auto.contains(true)
    def needWs: Boolean = ws.contains(true)
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
