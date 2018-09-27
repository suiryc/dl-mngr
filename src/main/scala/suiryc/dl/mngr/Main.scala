package suiryc.dl.mngr

import akka.actor.ActorSystem
import java.io.Closeable
import java.nio.file.Path
import javafx.application.Application
import javafx.stage.Stage
import monix.execution.Scheduler
import scala.concurrent.{ExecutionContextExecutor, Promise}
import suiryc.dl.mngr.I18N.Strings
import suiryc.dl.mngr.controllers.MainController
import suiryc.dl.mngr.model.NewDownloadInfo
import suiryc.scala.akka.CoreSystem
import suiryc.scala.io.SystemStreams
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.misc.Util
import suiryc.scala.sys.UniqueInstance
import suiryc.scala.sys.UniqueInstance.CommandResult

object Main {

  // Note: use 'lazy' for fields that indirectly trigger stdout/stderr writing
  // (e.g. through logger, or scala Console println etc), so that we can
  // redirect streams before it happens.

  val appPath: Path = Util.classLocation[this.type]

  def appPathRelative(other: String): Path = appPath.resolve(other)

  val statePath: Path = appPathRelative("state.json")

  lazy val settings = new Settings(appPathRelative("application.conf"))

  val versionedName: String = s"${suiryc.dl.mngr.Info.name} ${suiryc.dl.mngr.Info.version}" +
    suiryc.dl.mngr.Info.gitHeadCommit.map(v ⇒ s" ($v)").getOrElse("")

  // Streams
  private val streams = SystemStreams()

  // Promise to complete when we are ready to process command arguments
  private val promise = Promise[Unit]()

  // The running controller
  var controller: MainController = _

  private val parser: scopt.OptionParser[Params] = new scopt.OptionParser[Params](getClass.getCanonicalName) {
    def nonEmptyOrNone(s: String): Option[String] = Option(s).filterNot(_.trim.isEmpty)
    head(versionedName)
    help("help")
    opt[Unit]("auto").action { (_, c) ⇒
      c.copy(auto = true)
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
      c.copy(ioCapture = v)
    }
    opt[String]("unique-instance-id").action { (v, c) ⇒
      c.copy(uniqueInstanceId = v)
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
  }

  def main(args: Array[String]): Unit = {
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
        val ioCapture = params.ioCapture
        if (ioCapture) {
          SystemStreams.replace(
            SystemStreams.loggerOutput("stdout"),
            SystemStreams.loggerOutput("stderr", error = true)
          )
        }
        val uniqueInstanceId = params.uniqueInstanceId
        val processed = UniqueInstance.start(uniqueInstanceId, cmd _, args, promise.future, streams)
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
          }(Akka.dispatcher)
        }
        // 'launch' does not return until application is closed
        Application.launch(classOf[Main])

      case None ⇒
        sys.exit(-1)
    }
  }

  protected def cmd(args: Array[String]): CommandResult = {
    // Second parsing to actually process the arguments through unique instance.
    def process(): Unit = parser.parse(args, Params()).foreach { params ⇒
      // Sanity check: we should not be able to process command arguments if we
      // failed to create the controller.
      if (controller != null) {
        if (params.url.isDefined) {
          val dlInfo = NewDownloadInfo(
            auto = params.auto,
            uri = params.url,
            referrer = params.referrer,
            cookie = params.cookie,
            userAgent = params.userAgent,
            file = params.file,
            comment = params.comment
          )
          controller.addDownload(dlInfo)
        }
      }
    }

    try {
      process()
    } catch {
      case ex: Exception ⇒
        // Display the issue in the GUI when possible.
        Option(controller).foreach(_.displayError(
          title = None,
          contentText = Some(Strings.cliIssue),
          ex = ex
        ))
        // At worst caller will log it.
        throw ex
    }

    CommandResult(0, None)
  }

  object Akka {

    implicit val system: ActorSystem = CoreSystem.system
    implicit val dispatcher: ExecutionContextExecutor = system.dispatcher

  }

  lazy val scheduler: Scheduler = CoreSystem.scheduler

  def shutdown(stage: Stage): Unit = {
    JFXSystem.runLater(stage.close())
    shutdown()
  }

  def shutdown(): Unit = {
    UniqueInstance.stop()
    // Note: we share the same system
    JFXSystem.terminate(JFXSystem.dispatcher)
    ()
  }

  case class Params(
    auto: Boolean = false,
    comment: Option[String] = None,
    cookie: Option[String] = None,
    file: Option[String] = None,
    ioCapture: Boolean = true,
    referrer: Option[String] = None,
    uniqueInstanceId: String = "suiryc.dl-mngr",
    url: Option[String] = None,
    userAgent: Option[String] = None
  )

}

class Main extends Application {

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
