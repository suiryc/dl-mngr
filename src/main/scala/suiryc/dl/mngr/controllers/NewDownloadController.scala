package suiryc.dl.mngr.controllers

import java.io.File
import java.net.URI
import java.nio.file.{Files, Path, Paths}
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.input.Clipboard
import javafx.stage.{FileChooser, Stage, Window}
import suiryc.dl.mngr.model.{Download, NewDownloadInfo}
import suiryc.dl.mngr.util.{Http, Icons}
import suiryc.dl.mngr.{DownloadManager, I18N, Main, Settings}
import suiryc.scala.io.PathsEx
import suiryc.scala.io.RichFile._
import suiryc.scala.javafx.beans.binding.BindingsEx
import suiryc.scala.javafx.beans.property.ConfigEntryProperty
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.scene.control.{Dialogs, TextFieldWithButton}
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.javafx.stage.{PathChoosers, StagePersistentView, Stages}
import suiryc.scala.misc.Units
import suiryc.scala.settings.ConfigEntry

class NewDownloadController extends StagePersistentView {

  import I18N.Strings
  import NewDownloadController._

  @FXML
  protected var uriField: TextField = _

  @FXML
  protected var referrerField: TextField = _

  @FXML
  protected var cookieField: TextField = _

  @FXML
  protected var userAgentField: TextField = _

  @FXML
  protected var folderField: TextField = _

  @FXML
  protected var filenameField: TextFieldWithButton = _

  @FXML
  protected var startAutomaticallyField: CheckBox = _

  @FXML
  protected var insertFirstField: CheckBox = _

  @FXML
  protected var commentField: TextArea = _

  private var dialog: Dialog[_] = _

  private lazy val stage: Stage = Stages.getStage(dialog)

  private var dlMngr: DownloadManager = _

  private var dlInfo: NewDownloadInfo = _

  private var result: Option[Result] = None

  def initialize(dialog: Dialog[_], dlMngr: DownloadManager, dlInfo: NewDownloadInfo): Unit = {
    this.dialog = dialog
    this.dlMngr = dlMngr
    this.dlInfo = dlInfo

    filenameField.getButtons.head.arrowButton.getStyleClass.add("icon-sync")

    Icons.setIcons(stage.getScene.getRoot)

    folderField.setText(Main.settings.downloadsPath.get.toString)

    dlInfo.uri match {
      case Some(uri) ⇒
        uriField.setText(uri)

      case None ⇒
        // Get URI from clipboard when applicable.
        val clipboard = Clipboard.getSystemClipboard
        Option(clipboard.getUrl).map(_.trim).filterNot(_.isEmpty).orElse {
          Option(clipboard.getString).map(_.trim).filterNot(_.isEmpty)
        }.flatMap { s ⇒
          try {
            Some(Http.getURI(s)).filter { v ⇒
              (v.getScheme != null) && (v.getPath != null)
            }
          } catch {
            case _: Exception ⇒
              None
          }
        }.foreach { uri ⇒
          uriField.setText(uri.toString)
        }
    }
    dlInfo.referrer.foreach(referrerField.setText)
    dlInfo.cookie.foreach(cookieField.setText)
    dlInfo.userAgent.foreach(userAgentField.setText)
    val comment = List(
      dlInfo.sizeHint.filter(_ >= 0).map(Units.storage.toHumanReadable(_)),
      dlInfo.comment
    ).flatten.mkString("\n")
    if (comment.nonEmpty) commentField.setText(comment)
    // Take into account given file(name)
    dlInfo.file.map(Paths.get(_)) match {
      case Some(path) ⇒
        // If this is an absolute path, use the parent folder
        if (path.isAbsolute) folderField.setText(path.getParent.toString)
        // In any case, use the filename
        updateFilename(Some(path.getFileName.toString))

      case None ⇒
        updateFilename(None)
    }

    startAutomaticallyField.setSelected(startAutomatically.get)
    BindingsEx.bind(startAutomatically, startAutomaticallyField.selectedProperty) {
      startAutomaticallyField.isSelected
    }

    insertFirstField.setSelected(insertFirst.get)
    BindingsEx.bind(insertFirst, insertFirstField.selectedProperty) {
      insertFirstField.isSelected
    }

    val buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    buttonOk.addEventFilter(ActionEvent.ACTION, (event: ActionEvent) ⇒ {
      result = checkForm(auto = false)
      if (result.isEmpty) event.consume()
    })

    if (dlInfo.auto) result = checkForm(auto = dlInfo.auto)
  }

  /** Restores (persisted) view. */
  override protected def restoreView(): Unit = {
    Stages.onStageReady(stage, first = false) {
      // Restore stage location
      Stages.setMinimumDimensions(stage)
      stageLocation.opt.foreach { loc ⇒
        Stages.setLocation(stage, loc, setSize = true)
      }
    }(JFXSystem.dispatcher)
  }

  /** Persists view (stage location, ...). */
  override protected def persistView(): Unit = {
    // Persist stage location
    // Note: if iconified, resets it
    stageLocation.set(Stages.getLocation(stage).orNull)
  }

  protected def checkForm(auto: Boolean): Option[Result] = {
    val referrer = getReferrer
    val cookie = getCookie
    val userAgent = getUserAgent
    getURI.flatMap { uri ⇒
      dlMngr.findDownload(uri) match {
        case Some(download) ⇒
          // This download (URI) already exists
          val r = if (auto) {
            // In automatic mode, resume download
            Some(false)
          } else {
            restartOrResume(
              header = Strings.downloadAlreadyUri,
              content = uri.toString,
              canRestart = download.canRestart,
              canResume = download.canResume,
              defaultRestart = false
            )
          }
          r.map { restart ⇒
            Result(
              download = download,
              restart = restart,
              select = !auto
            )
          }

        case None ⇒
          // First check for special characters to replace in filename
          val path0 = Paths.get(getPathRaw(sanitize = true))
          val pathRaw0 = getPathRaw(sanitize = false)
          val pathRaw1 = Paths.get(pathRaw0)
          if (pathRaw1 != path0) {
            Dialogs.information(
              owner = Option(stage),
              title = None,
              headerText = Some(Strings.reservedChars),
              contentText = Some(path0.toString)
            )
          }
          // Then check whether target/temporary file already exists
          val temporary = Main.settings.getTemporaryFile(path0)
          val temporaryExists = temporary.exists(_.toFile.exists)
          val exists = (List(path0) ::: temporary.toList).filter(_.toFile.exists)
          if (exists.nonEmpty) {
            // A file already exists
            val (path, r) = if (auto) {
              // In automatic mode, don't restart, but rename download if needed.
              val path = findAvailablePath(path0)
              (path, Some(false))
            } else {
              // TODO: also propose to rename ?
              val r = restartOrResume(
                header = Strings.downloadAlreadyFile,
                content = exists.mkString("\n"),
                canRestart = true,
                // We can only resume the file to which we are to write (temporary or not)
                canResume = temporary.isEmpty || temporaryExists,
                defaultRestart = true
              )
              (path0, r)
            }
            // Inform if target was renamed
            if (path != path0) {
              Dialogs.information(
                owner = Option(stage),
                title = None,
                headerText = Some(Strings.renamedFile),
                contentText = Some(s"$path0\n$path")
              )
            }
            r.map { restart ⇒
              // Upon restarting, delete existing files
              if (restart) {
                path.toFile.delete()
                temporary.foreach(_.toFile.delete())
              }
              Result(
                download = None,
                uri = uri,
                referrer = referrer,
                cookie = cookie,
                userAgent = userAgent,
                path = path,
                reused = !restart,
                // For 'restarting' we actually deleted the file (so no need to 'restart' its content)
                restart = false,
                insertFirst = insertFirstField.isSelected,
                start = startAutomaticallyField.isSelected,
                select = !auto
              )
            }
          } else {
            Some(Result(
              download = None,
              uri = uri,
              referrer = referrer,
              cookie = cookie,
              userAgent = userAgent,
              path = path0,
              reused = false,
              restart = false,
              insertFirst = insertFirstField.isSelected,
              start = startAutomaticallyField.isSelected,
              select = !auto
            ))
          }
      }
    }
  }

  def onFilenameRefresh(): Unit = {
    filenameField.textField.requestFocus()
    updateFilename(None)
  }

  def onFileSelect(@deprecated("unused","") event: ActionEvent): Unit = {
    val fileChooser = new FileChooser()
    fileChooser.setTitle(dialog.getTitle)
    fileChooser.getExtensionFilters.addAll(
      new FileChooser.ExtensionFilter("*.*", "*.*")
    )
    PathChoosers.setInitialPath(fileChooser, getPath.toFile)
    Option(fileChooser.showSaveDialog(stage)).foreach { selectedFile ⇒
      folderField.setText(selectedFile.getParent)
      filenameField.setText(selectedFile.getName)
    }
  }

  def restartOrResume(header: String, content: String, canResume: Boolean, canRestart: Boolean, defaultRestart: Boolean): Option[Boolean] = {
    val buttonRestart = new ButtonType(Strings.restart)
    val buttonResume = new ButtonType(Strings.resume)
    val buttonCancel = ButtonType.CANCEL
    val b1 = if (canRestart) Some(buttonRestart) else None
    val b2 = if (canResume) Some(buttonResume) else None
    val buttons = (if (defaultRestart) {
      b1.toList ::: b2.toList
    } else {
      b2.toList ::: b1.toList
    }) ::: List(buttonCancel)
    val defaultButton = (if (defaultRestart) b1 else b2).orElse(Some(buttonCancel))
    val r = Dialogs.confirmation(
      owner = Option(stage),
      title = None,
      headerText = Some(header),
      contentText = Some(content),
      buttons = buttons,
      defaultButton = defaultButton
    )
    r.flatMap {
      case `buttonRestart` ⇒ Some(true)
      case `buttonResume` ⇒ Some(false)
      case _ ⇒ None
    }
  }

  def getText(s: String): Option[String] = {
    Option(s).filterNot(_.trim.isEmpty)
  }

  def getURI(s: String): Option[URI] = {
    try {
      getText(s).map(Http.getURI)
    } catch {
      case ex: Exception ⇒
        Dialogs.error(
          owner = Option(stage),
          title = None,
          contentText = Some(Strings.invalidURI),
          ex = Some(ex)
        )
        None
    }
  }

  def getURI: Option[URI] = getURI(uriField.getText)

  def getReferrer: Option[URI] = getURI(referrerField.getText)

  def getCookie: Option[String] = getText(cookieField.getText)

  def getUserAgent: Option[String] = getText(userAgentField.getText)

  def getPathRaw(sanitize: Boolean): String = {
    val file0 = Option(filenameField.getText)
    val file = if (sanitize) file0.map(PathsEx.sanitizeFilename) else file0
    (Option(folderField.getText).toList ::: file.toList).mkString(File.separator)
  }

  def getPath: Path = Paths.get(getPathRaw(sanitize = true))

  def updateFilename(file: Option[String]): Unit = {
    file.orElse(getURI.map(Http.getFilename)).foreach { file ⇒
      val value = PathsEx.sanitizeFilename(file)
      filenameField.setText(value)
      filenameField.textField.positionCaret(value.length)
    }
  }

  /** Find path available (as download and temporary download file). */
  def findAvailablePath(path: Path): Path = {
    @scala.annotation.tailrec
    def loop(n: Int): Path = {
      val probe = if (n == 0) {
        path
      } else {
        val (base, ext) = path.toFile.baseAndExt
        val extOpt = Some(ext).filterNot(_.isEmpty)
        path.resolveSibling(s"$base ($n)${extOpt.map(v ⇒ s".$v").getOrElse("")}")
      }
      if (Files.exists(probe) || Main.settings.getTemporaryFile(probe).exists(Files.exists(_))) loop(n + 1)
      else probe
    }

    loop(0)
  }

}

object NewDownloadController {

  import I18N.Strings

  private val settingsKeyPrefix = "new-download"

  private val stageLocation = ConfigEntry.from[StageLocation](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_DL_MNGR, Settings.KEY_STAGE, settingsKeyPrefix, "location")

  private val startAutomatically = ConfigEntryProperty {
    ConfigEntry.from[Boolean](Main.settings.settings,
      Settings.KEY_SUIRYC, Settings.KEY_DL_MNGR, Settings.KEY_STAGE, settingsKeyPrefix, "start-automatically")
  }

  private val insertFirst = ConfigEntryProperty {
    ConfigEntry.from[Boolean](Main.settings.settings,
      Settings.KEY_SUIRYC, Settings.KEY_DL_MNGR, Settings.KEY_STAGE, settingsKeyPrefix, "insert-first")
  }

  protected case class Result(
    download: Option[Download],
    uri: URI,
    referrer: Option[URI],
    cookie: Option[String],
    userAgent: Option[String],
    path: Path,
    reused: Boolean,
    restart: Boolean,
    insertFirst: Boolean,
    start: Boolean,
    select: Boolean
  )

  object Result {

    def apply(download: Download, restart: Boolean, select: Boolean): Result = Result(
      download = Some(download),
      uri = download.uri,
      referrer = download.referrer,
      cookie = download.cookie,
      userAgent = download.userAgent,
      path = download.downloadFile.getPath,
      // Reused is true since we have an existing download
      reused = true,
      restart = restart,
      insertFirst = false,
      // Start download since we either choose resume or restart
      start = true,
      select = select
    )

  }

  /** Builds a dialog out of this controller. */
  def buildDialog(mainController: MainController, owner: Window, dlMngr: DownloadManager, dlInfo: NewDownloadInfo): Option[Dialog[Option[Download]]] = {
    val dialog = new Dialog[Option[Download]]()
    Stages.initOwner(dialog, owner)
    dialog.setTitle(Strings.addDownload)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/new-download.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load[Node]())
    val controller = loader.getController[NewDownloadController]
    controller.initialize(dialog, dlMngr, dlInfo)

    // Note: we must set the result converter in order to process the result
    // (be it in automatic mode or not)
    dialog.setResultConverter(resultConverter(mainController, controller) _)

    if (!dlInfo.auto || controller.result.isEmpty) {
      Dialogs.addPersistence(dialog, controller)

      // Bring to front once shown
      dialog.showingProperty.listen2 { (cancellable, v) ⇒
        if (v) {
          cancellable.cancel()
          val stage = Stages.getStage(dialog)
          // 'toFront' does not bring the dialog on top of other windows (other
          // than those of the application). Temporary 'setAlwaysOnTop' does.
          stage.requestFocus()
          stage.setAlwaysOnTop(true)
          stage.setAlwaysOnTop(false)
        }
      }

      Some(dialog)
    } else {
      dialog.close()
      None
    }
  }

  def resultConverter(mainController: MainController, controller: NewDownloadController)(@deprecated("unused","") buttonType: ButtonType): Option[Download] = {
    // Note: controller result is only set upon "OK"
    controller.result.map { result ⇒
      val dlMngr = controller.dlMngr
      val download = result.download.getOrElse {
        dlMngr.addDownload(
          uri = result.uri,
          referrer = result.referrer,
          cookie = result.cookie,
          userAgent = result.userAgent,
          save = result.path,
          sizeHint = controller.dlInfo.sizeHint.filter(_ >= 0),
          reused = result.reused,
          insertFirst = result.insertFirst
        )
      }
      if (result.start) dlMngr.resumeDownload(download.id, reusedOpt = Some(result.reused), restart = result.restart)
      if (result.download.isEmpty) mainController.addDownload(download.id, result.insertFirst, result.select)
      download
    }
  }

}
