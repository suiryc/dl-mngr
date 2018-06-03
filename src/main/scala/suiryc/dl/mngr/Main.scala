package suiryc.dl.mngr

import akka.actor.ActorSystem
import java.nio.file.Path
import javafx.application.Application
import javafx.stage.Stage
import monix.execution.Scheduler
import scala.concurrent.{ExecutionContextExecutor, Promise}
import suiryc.dl.mngr.I18N.Strings
import suiryc.dl.mngr.controllers.MainController
import suiryc.dl.mngr.model.NewDownloadInfo
import suiryc.scala.akka.CoreSystem
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.misc.Util
import suiryc.scala.sys.UniqueInstance

object Main {

  val appPath: Path = Util.classLocation[this.type]

  val statePath: Path = appPath.resolve("state.json")

  val settings = new Settings(appPath.resolve("application.conf"))

  val versionedName: String = s"${suiryc.dl.mngr.Info.name} ${suiryc.dl.mngr.Info.version}" +
    suiryc.dl.mngr.Info.gitHeadCommit.map(v ⇒ s" ($v)").getOrElse("")

  // Promise to complete when we are ready to process command arguments
  private val promise = Promise[Unit]()
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
        UniqueInstance.start(params.uniqueInstanceId, cmd, args, promise.future)
        // 'launch' does not return until application is closed
        Application.launch(classOf[Main])

      case None ⇒
        sys.exit(-1)
    }
  }

  protected def cmd(args: Array[String]): Int = {
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

    0
  }

  object Akka {

    implicit val system: ActorSystem = CoreSystem.system
    implicit val dispatcher: ExecutionContextExecutor = system.dispatcher

  }

  val scheduler: Scheduler = CoreSystem.scheduler

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
