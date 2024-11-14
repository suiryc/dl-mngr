package suiryc.dl.mngr.controllers

import akka.actor.{Actor, ActorRef, Props}
import com.sun.javafx.scene.control.behavior.CellBehaviorBase
import com.typesafe.scalalogging.StrictLogging
import javafx.beans.property.{SimpleBooleanProperty, SimpleLongProperty, SimpleObjectProperty, SimpleStringProperty}
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.SpinnerValueFactory.IntegerSpinnerValueFactory
import javafx.scene.{Node, Parent, Scene}
import javafx.scene.control._
import javafx.scene.input._
import javafx.scene.layout.{Pane, StackPane}
import javafx.scene.text.{Font, Text}
import javafx.stage.{FileChooser, Modality, Stage, WindowEvent}
import javafx.util.StringConverter
import monix.execution.Cancelable
import suiryc.dl.mngr.model._
import suiryc.scala.misc.Units
import suiryc.dl.mngr.{DownloadManager, I18N, Info, Main, Settings}
import suiryc.dl.mngr.I18N.Strings
import suiryc.dl.mngr.util.{Http, Icons}
import suiryc.scala.RichOption._
import suiryc.scala.concurrent.{Cancellable, RichFuture}
import suiryc.scala.javafx.beans.binding.BindingsEx
import suiryc.scala.javafx.beans.value.RichObservableValue
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.collections.RichObservableList._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.css.Styleables
import suiryc.scala.javafx.scene.{Graphics, Nodes, Styles}
import suiryc.scala.javafx.scene.control.{Dialogs, Panes, Spinners, TableCellEx, TableViews}
import suiryc.scala.javafx.scene.control.skin.SplitPaneSkinEx
import suiryc.scala.javafx.scene.text.Fonts
import suiryc.scala.javafx.stage.{PathChoosers, StageLocationPersistentView, Stages}
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.settings.ConfigEntry
import suiryc.scala.util.CallsThrottler

import java.io.{PrintWriter, StringWriter}
import java.net.URI
import java.nio.file.Path
import java.text.SimpleDateFormat
import java.util.UUID
import scala.annotation.{nowarn, unused}
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.math.BigDecimal.RoundingMode
import scala.util.{Failure, Success}


class MainController
  extends StageLocationPersistentView(MainController.stageLocation, first = true)
  with ObservableLogs
  with StrictLogging
{

  import MainController._

  @FXML
  protected var downloadsMenu: Menu = _

  @FXML
  @unused
  protected var downloadsStopAllMenu: MenuItem = _

  @FXML
  @unused
  protected var downloadsResumeAllMenu: MenuItem = _

  @FXML
  @unused
  protected var downloadsRemoveCompletedMenu: MenuItem = _

  @FXML
  @unused
  protected var downloadsRemoveMenu: MenuItem = _

  @FXML
  protected var rateLimitField: TextField = _

  @FXML
  protected var rateLimitUnitField: ChoiceBox[Units.Unit] = _

  @FXML
  protected var splitPane: SplitPane = _

  @FXML
  protected var downloadsTable: TableView[UUID] = _

  @FXML
  protected var logsTable: TableView[LogEntry] = _

  @FXML
  protected var dlPropertiesTab: Tab = _

  @FXML
  protected var dlPropertiesScrollPane: ScrollPane = _

  @FXML
  protected var dlURIDebugButton: Button = _

  @FXML
  protected var dlURIField: TextField = _

  @FXML
  protected var dlIPField: TextField = _

  @FXML
  protected var dlServerLink: Hyperlink = _

  @FXML
  protected var dlServerMaxCnxField: Spinner[Integer] = _

  @FXML
  protected var dlSiteLink: Hyperlink = _

  @FXML
  protected var dlSiteMaxCnxField: Spinner[Integer] = _

  @FXML
  protected var dlSiteMaxSegmentsField: Spinner[Integer] = _

  @FXML
  protected var dlReferrerField: TextField = _

  @FXML
  protected var dlCookieField: TextField = _

  @FXML
  protected var dlUserAgentField: TextField = _

  @FXML
  protected var dlFolderField: TextField = _

  @FXML
  protected var dlFileField: TextField = _

  @FXML
  protected var dlFileSelectButton: Button = _

  @FXML
  protected var dlSizeLabel: Label = _

  @FXML
  protected var dlLastModifiedLabel: Label = _

  @FXML
  protected var allDlRunningLabel: Label = _

  @FXML
  protected var allDlProgressLabel: Label = _

  @FXML
  protected var allDlSpeedLabel: Label = _

  private val clipboard = Clipboard.getSystemClipboard

  private val CTRL_C = new KeyCodeCombination(KeyCode.C, KeyCombination.CONTROL_DOWN)

  private val columnDownloadIndex = new TableColumn[UUID, String]("#")

  private val columnDownloadFile = new TableColumn[UUID, UUID](Strings.file)
  private val columnDownloadSize = new TableColumn[UUID, UUID](Strings.size)
  private val columnDownloadDownloaded = new TableColumn[UUID, UUID](Strings.downloaded)
  private val columnDownloadSpeed = new TableColumn[UUID, String](Strings.speed)
  private val columnDownloadEta = new TableColumn[UUID, String](Strings.eta)
  private val columnDownloadSegments = new TableColumn[UUID, String](Strings.segments)

  private val downloadsColumns = List(
    "idx" -> columnDownloadIndex,
    "file" -> columnDownloadFile,
    "size" -> columnDownloadSize,
    "downloaded" -> columnDownloadDownloaded,
    "speed" -> columnDownloadSpeed,
    "eta" -> columnDownloadEta,
    "segments" -> columnDownloadSegments
  )

  private val columnLogTime = new TableColumn[LogEntry, LogEntry](Strings.time)
  private val columnLogMessage = new TableColumn[LogEntry, LogEntry](Strings.message)

  private val logsColumns = List(
    "time" -> columnLogTime,
    "message" -> columnLogMessage
  )

  lazy protected val stage: Stage = splitPane.getScene.getWindow.asInstanceOf[Stage]

  private val jfxThrottler = CallsThrottler(JFXSystem.scheduler)

  private var actor: ActorRef = _

  @volatile
  private var downloadData: Map[UUID, DownloadData] = Map.empty

  def initialize(state: State): Unit = {
    refreshLogs()
    addLog(LogKind.Info, s"Started name=<${Info.name}> version=<${Info.version}>${
      Info.commitId.map(v => s" gitCommit=<$v>").getOrElse("")
    } buildTime=<${Main.buildTimeString}>")
    state.stage.setTitle(Main.versionedName)

    // Note: make the actor name unique (with timestamp) so that it can be
    // recreated later.
    actor = JFXSystem.newJFXActor(
      Props(new ControllerActor(state)),
      s"dl-mngr-main@${System.currentTimeMillis}"
    )

    // We sometimes need to get the current state. So store it as user data.
    state.save()

    // Inject icons in menu and panes
    Icons.setIcons(stage.getScene.getRoot)

    // We would like to update "Downloads" menu items upon showing (to only
    // enable applicable actions). We could also follow table items list and
    // each download state, but that would be a bit overkill.
    // Unfortunately, when enabling/disabling items, after a while those items
    // remain greyed out even if enabled ...
    //downloadsStopAllMenu.getParentMenu.setOnShowing { _ =>
    //  val _data = getDownloadsData
    //  enableMenuStop(downloadsStopAllMenu, _data)
    //  enableMenuResume(downloadsResumeAllMenu, _data)
    //  enableMenuRemoveCompleted(downloadsRemoveCompletedMenu, _data)
    //  enableMenuRemove(downloadsRemoveMenu, _data)
    //}

    // We wish to disable mouse events in the custom menu item (but not the
    // nodes within). rateLimitField and rateLimitUnitField are children of an
    // HBox, and its parent will be set upon first displaying the menu.
    // First wait for this to happen.
    rateLimitField.getParent.parentProperty.listen { parent =>
      // Now filter all mouse events targeted at this parent.
      parent.addEventFilter(MouseEvent.ANY, (event: MouseEvent) => {
        if (parent.eq(event.getTarget)) event.consume()
      })
    }
    def getRateLimitValue: Long = {
      try {
        Option(rateLimitField.getText).map(_.toLong).filter(_ >= 0L).getOrElse(0L)
      } catch {
        case _: Exception => 0L
      }
    }
    def updateRateLimiter(): Unit = {
      val value = getRateLimitValue
      val unit = rateLimitUnitField.getValue
      val bytesPerSecond = value * unit.factor
      getState.dlMngr.setRateLimit(bytesPerSecond)
      Main.settings.rateLimitValue.set(value)
      Main.settings.rateLimitUnit.set(unit.label)
      refreshAllDlSpeed()
      // If rate is 0, reset the field text (in case the value was actually
      // invalid).
      if (value == 0) rateLimitField.setText("0")
    }
    rateLimitField.setOnAction { _ =>
      updateRateLimiter()
    }
    downloadsMenu.setOnHidden { _ =>
      updateRateLimiter()
    }
    rateLimitField.setMinWidth(computeTextWidth("__9999__"))
    rateLimitField.setPrefWidth(computeTextWidth("__9999__"))
    rateLimitField.setMaxWidth(computeTextWidth("__9999__"))
    val rateLimitUnitConverter = new StringConverter[Units.Unit] {
      override def toString(v: Units.Unit): String = s"${v.label}/s"
      override def fromString(v: String): Units.Unit = {
        Units.storage.units_Binary.find(_.label == v).get
      }
    }
    rateLimitUnitField.setConverter(rateLimitUnitConverter)
    rateLimitUnitField.getItems.setAll(Units.storage.kibi, Units.storage.mebi)

    rateLimitField.setText(Main.settings.rateLimitValue.get.toString)
    val unit = try {
      rateLimitUnitConverter.fromString(Main.settings.rateLimitUnit.get)
    } catch {
      case _: Exception => Units.storage.kibi
    }
    rateLimitUnitField.getSelectionModel.select(unit)
    updateRateLimiter()

    downloadsColumns.foreach(_._2.setSortable(false))
    columnDownloadIndex.setCellFactory { _ =>
      new TableCellEx[UUID, String] {
        override protected def itemText(item: String): String = {
          val index = getIndex
          if (index >= 0) (index + 1).toString
          else null
        }
      }
    }
    columnDownloadIndex.setMinWidth(computeTextWidth("_99_"))
    columnDownloadFile.setCellValueFactory { data =>
      new SimpleObjectProperty[UUID](data.getValue)
    }
    columnDownloadFile.setCellFactory { _ =>
      new TableCell[UUID, UUID] {
        override def updateItem(item: UUID, empty: Boolean): Unit = {
          super.updateItem(item, empty)
          // Notes:
          // We will bind this cell text/graphic to the download path/icon state.
          // So before changing the value, we must unbind it.
          //
          // Changing the text will re-set the cell children (text and graphic).
          // It is thus better to change the graphic first, otherwise in some
          // situations (e.g. changing the order of items) this triggers
          // unwanted behaviour:
          //  -> cell C1 and C2 display items I1 and I2
          //  -> cell C1 is assigned item I2
          //   -> C1 text is set from I2
          //    -> C1 children are re-set: text and I1 icon
          //   -> C1 graphic is set to I2 icon
          //    -> I2 icon is removed from C2 (now a child of C1), while still
          //       being valued as C2 graphic
          //    -> I1 icon is now parentless
          //  -> cell C2 is assigned item I1
          //   -> C2 text is set from I1
          //    -> C2 children are re-set: text and I2 icon (still valued as
          //       graphic)
          //     -> I2 icon is removed from C1 (now child of C2), while still
          //        being valued as C1 graphic
          //   -> C2 graphic is set to I1 icon
          //    -> I2 icon is now parentless
          // In the end C1 displays I2 but C2 is missing the I1 icon.
          // Setting the graphic first prevents this.
          graphicProperty.unbind()
          textProperty.unbind()
          tooltipProperty.unbind()
          val opt = if (!empty) getDownloadData(item) else None
          opt match {
            case Some(data) =>
              graphicProperty.bind(data.stateIcon)
              BindingsEx.bind(textProperty, data.path) {
                data.path.get.getFileName.toString
              }
              BindingsEx.bind(tooltipProperty, data.path) {
                new Tooltip(data.path.get.toString)
              }
              ()

            case None =>
              setGraphic(null)
              setText(null)
              setTooltip(null)
          }
        }
      }
    }
    columnDownloadFile.setMinWidth(computeTextWidth("MMMMMMMMMMMMMMMM") + 24)
    columnDownloadSize.setCellValueFactory { data =>
      new SimpleObjectProperty[UUID](data.getValue)
    }
    columnDownloadSize.setCellFactory { _ =>
      new TableCell[UUID, UUID] {
        override def updateItem(item: UUID, empty: Boolean): Unit = {
          super.updateItem(item, empty)
          graphicProperty.unbind()
          textProperty.unbind()
          val opt = if (!empty) getDownloadData(item) else None
          opt match {
            case Some(data) =>
              BindingsEx.bind(graphicProperty, data.sizeIcon) {
                data.sizeIcon.get
              }
              BindingsEx.bind(textProperty, data.size) {
                data.size.get
              }

            case None =>
              setGraphic(null)
              setText(null)
          }
        }
      }
    }
    columnDownloadSize.setMinWidth(computeTextWidth("_9999.9_MiB_"))
    columnDownloadDownloaded.setCellValueFactory { data =>
      new SimpleObjectProperty[UUID](data.getValue)
    }
    columnDownloadDownloaded.setCellFactory { _ =>
      new TableCellEx[UUID, UUID] {
        getStyleClass.add("table-cell-downloaded")
        override protected def itemText(item: UUID): String = null
        override protected def itemGraphic(item: UUID): Node = {
          getDownloadData(item).map(_.downloadedProgressPane).orNull
        }
      }
    }
    columnDownloadDownloaded.setMinWidth(computeTextWidth("_999.9%_[9999.9_MiB]_"))

    columnDownloadSpeed.setCellValueFactory { data =>
      getDownloadData(data.getValue).map(_.rate).getOrElse {
        new SimpleStringProperty()
      }
    }
    columnDownloadSpeed.setMinWidth(computeTextWidth("_9999.9_MiB/s_"))

    columnDownloadEta.setCellValueFactory { data =>
      getDownloadData(data.getValue).map(_.eta).getOrElse {
        new SimpleStringProperty()
      }
    }
    columnDownloadEta.setMinWidth(computeTextWidth("_99:99:99_"))

    columnDownloadSegments.setCellValueFactory { data =>
      getDownloadData(data.getValue).map(_.segments).getOrElse {
        new SimpleStringProperty()
      }
    }
    columnDownloadSegments.setMinWidth(computeTextWidth("_99/99_"))

    logsColumns.foreach(_._2.setSortable(false))
    columnLogTime.setCellValueFactory { data =>
      new SimpleObjectProperty[LogEntry](data.getValue)
    }
    columnLogTime.setCellFactory { _ =>
      new TableCellEx[LogEntry, LogEntry] {
        override protected def itemText(item: LogEntry): String = {
          item.time.format(Main.timeFormatter)
        }
      }
    }
    columnLogTime.setMinWidth(computeTextWidth("_9999-99-99_99:99:99.999_"))
    // We want to be able to sort logs by time, especially so that we can
    // display recent logs at the top of the view.
    // The way logs are created, we only expect increasing time.
    // In the case two logs have the exact same time, use an increasing id to
    // properly sort them.
    columnLogTime.setSortable(true)
    columnLogTime.setComparator((v1, v2) => {
      val c = v1.time.compareTo(v2.time)
      if (c == 0) (v1.id - v2.id).intValue
      else c
    })

    columnLogMessage.setCellValueFactory { data =>
      new SimpleObjectProperty[LogEntry](data.getValue)
    }
    columnLogMessage.setCellFactory { _ =>
      new TableCellEx[LogEntry, LogEntry] {
        override protected def itemText(item: LogEntry): String = {
          item.message
        }
        override protected def itemGraphic(item: LogEntry): Node = {
          item.kind match {
            case LogKind.Error => Icons.exclamationTriangle().pane
            case LogKind.Warning => Icons.exclamationTriangle(styleClass = List("icon-exclamation-triangle-warning")).pane
            case _ => Icons.infoCircle().pane
          }
        }
        override protected def updateItem(item: LogEntry, empty: Boolean): Unit = {
          super.updateItem(item, empty)
          val tooltip = Option(item).flatMap { item =>
            val sw = new StringWriter()
            item.tooltip.foreach(sw.append)
            item.error.foreach { ex =>
              if (sw.getBuffer.length > 0) sw.append("\n")
              val pw = new PrintWriter(sw)
              ex.printStackTrace(pw)
            }
            if (sw.getBuffer.length > 0) Some(new Tooltip(sw.toString))
            else None
          }.orNull
          setTooltip(tooltip)
        }
      }
    }

    downloadsTable.setContextMenu(downloadsContextMenu)
    downloadsTable.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
    downloadsTable.getSelectionModel.selectedItemProperty.listen {
      refreshLogs()
      refreshDlProperties()
    }

    // Note: 'KEY_PRESSED' and 'KEY_TYPED' are continuously triggered while key
    // remain pressed. 'KEY_RELEASED' is only triggered upon releasing the key.
    // But only 'KEY_TYPED' will give the actual character when combining
    // multiple keys (e.g. 'Shift'+'=' gives '+'). And only 'KEY_PRESSED' or
    // 'KEY_RELEASED' capture keys that do not produce character (like arrows).
    downloadsTable.addEventHandler(KeyEvent.KEY_TYPED, (event: KeyEvent) => {
      event.getCharacter match {
        case "+" =>
          selectedDownloadsData.filter { data =>
            data.download.isNetworkActive || data.download.canResume
          }.foreach { data =>
            getState.dlMngr.addDownloadConnection(data.download.id)
          }

        case "-" =>
          selectedDownloadsData.filter(_.download.isDownloading).foreach { data =>
            getState.dlMngr.removeDownloadConnection(data.download.id)
          }

        case _ =>
      }
    })
    downloadsTable.addEventHandler(KeyEvent.KEY_PRESSED, (event: KeyEvent) => {
      if (event.isControlDown) {
        event.getCode match {
          case KeyCode.PAGE_UP =>
            moveDownloads(selectedDownloads, up = true, most = true)
            event.consume()

          case KeyCode.UP =>
            moveDownloads(selectedDownloads, up = true, most = event.isShiftDown)
            event.consume()

          case KeyCode.DOWN =>
            moveDownloads(selectedDownloads, up = false, most = event.isShiftDown)
            event.consume()

          case KeyCode.PAGE_DOWN =>
            moveDownloads(selectedDownloads, up = false, most = true)
            event.consume()

          case _ =>
        }
      } else if (event.getCode == KeyCode.DELETE) {
        onDownloadsRemove(force = event.isShiftDown)
      }
    })
    // Refresh status bar when changing items.
    downloadsTable.getItems.listen { change =>
      // Only DL being added/removed matters here.
      @scala.annotation.tailrec
      def loop(): Boolean = {
        if (change.next()) {
          if (change.wasAdded() || change.wasRemoved()) true
          else loop()
        } else false
      }

      if (loop()) {
        refreshAllDlRunning()
        refreshAllDlProgress()
      }
    }
    TableViews.trackRows(downloadsTable, downloadRowUpdated)

    logsTable.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
    // Handle 'Ctrl-c' to copy log(s).
    logsTable.addEventHandler(KeyEvent.KEY_PRESSED, (event: KeyEvent) => {
      if (CTRL_C.`match`(event)) Option(logsTable.getSelectionModel.getSelectedItems.asScala.toList).foreach(copyDownloadLogsToClipboard)
    })
    // Note:
    // Previously only download logs were displayed in logsTable. Since they
    // are set asynchronously, a race condition was possible:
    // 1. Download entry is selected
    // 2.1. Download logs are about to be set in logsTable (through JavaFX
    //      thread)
    // 2.2. Download entry is de-selected (possibly successfully completed and
    //      automatically removed)
    // 2.3. logsTable is emptied
    // 2.4. Logs from 2.1. are finally set in logsTable
    // 3. logsTable displays logs from de-selected download
    // A workaround was to listen for logsTable items changes, and refresh them
    // (to empty the table) if there were items while no download was selected.
    // Now that we also handle session logs (displayed when no download is
    // selected), those would simply overwrite unwanted logs when applicable.

    Spinners.handleEvents(dlServerMaxCnxField)
    Spinners.handleEvents(dlSiteMaxCnxField)
    Spinners.handleEvents(dlSiteMaxSegmentsField)
  }

  private def computeTextWidth(s: String): Double = Fonts.textWidth(Font.getDefault, s)

  private def cancelSubscription(v: Option[Any]): Unit = {
    v match {
      case Some(cancellable: Cancellable) => cancellable.cancel()
      case _ =>
    }
  }

  private def copyDownloadLogsToClipboard(entries: List[LogEntry]): Unit = {
    val text = entries.map { entry =>
      val lines = s"${entry.time.format(Main.timeFormatter)} [${entry.kind}] ${entry.message}" :: entry.error.map { ex =>
        val sw = new StringWriter()
        val pw = new PrintWriter(sw)
        ex.printStackTrace(pw)
        sw.toString.trim
      }.toList
      lines.mkString("", "\n", "\n")
    }.mkString("")
    val content = new ClipboardContent()
    content.putString(text)
    clipboard.setContent(content)
    ()
  }

  private def refreshLogs(): Unit = {
    // Cancel previous subscriptions if any
    cancelSubscription(Option(logsTable.getUserData))

    // Fix minimum width; will be updated when setting cells content
    columnLogMessage.setMinWidth(computeTextWidth("MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM"))

    val observableLogs: ObservableLogs = selectedDownloadData.map(_.download.info).getOrElse(this)
    def getLogs(l: java.util.List[_ <: LogEntry]): List[LogEntry] = {
      val logs0 = l.asScala.toList
      if (!Main.settings.debug.get) {
        logs0.filter(_.kind != LogKind.Debug)
      } else {
        logs0
      }
    }
    def setLogs(): Unit = {
      // If we are called to initially populate the table, logs may be
      // changed while we work with the list. To prevent concurrent access
      // issues, lock the logs.
      val logs = observableLogs.withLogs(getLogs)
      // Note: we may not be in the JavaFX thread yet (e.g. dealing with
      // logs changes).
      JFXSystem.run {
        logsTable.getItems.setAll(logs: _*)
        // Re-sort the table.
        logsTable.sort()
        updateLogMessageMinWidth(logs)
      }
    }

    def updateLogMessageMinWidth(els: List[LogEntry]): Unit = {
      import scala.Ordering.Double.TotalOrdering

      if (els.nonEmpty) {
        val textWidth = els.map(entry => computeTextWidth(entry.message)).max
        // To compute the minimum width, we need the maximum text width and
        // any 'extra' (padding/insets) applied on the TableCell.
        // The commented code below can be used to compute those.
        //def insetsWidth(insets: Insets): Double = insets.getLeft + insets.getRight
        //val extraWidth = insetsWidth(cell.getPadding) + insetsWidth(cell.getInsets) + insetsWidth(cell.getLabelPadding)
        // But the very first built cells (that can be accessed in the
        // CellFactory above for example) do not have any padding/insets
        // yet ... (and 'runLater' does not help).
        // Keep it simple and use a hard-coded value: 10 on Windows and ~12
        // on Linux.
        val extraWidth = 12.0
        val minWidth = math.max(Graphics.iconSize + textWidth + extraWidth, columnLogMessage.getMinWidth)
        // Note: if only setting minimum width, often the actual width does
        // not change (when it was bigger before re-setting log entries)
        // until interaction is done (e.g. resizing window).
        // Setting max width (along with preferred width) helps applying the
        // target width right now, as is done below (in restoreView).
        columnLogMessage.setMinWidth(minWidth)
      }
    }

    // Listen to logs changes.
    // Notes:
    // We only expect entries to be added, which can be propagated easily in
    // the table. For any other change, re-set all the items.
    // In either case, update the column minimum width accordingly.
    // Don't forget to delegate UI changes to the JavaFX thread.
    // As indicated in 'logs' field comments, no other change must happen
    // while we iterate over added elements: thus only switch to the JavaFX
    // thread when we are done with the change content, so that handling
    // changes is done in the caller thread (which holds a lock).
    val logsCancellable = observableLogs.logs.listen { change =>
      @scala.annotation.tailrec
      def loop(): Boolean = {
        if (change.next()) {
          if (change.wasPermutated() || change.wasUpdated() || change.wasRemoved() || !change.wasAdded()) {
            setLogs()
            false
          } else {
            val logs = getLogs(change.getAddedSubList)
            if (logs.nonEmpty) {
              JFXSystem.run {
                // We only expect adding at the end. Adding at position
                // 'change.getFrom' fails when filtering debug messages.
                logsTable.getItems.addAll(logs.asJava)
                updateLogMessageMinWidth(logs)
              }
            }
            loop()
          }
        } else {
          true
        }
      }

      val resort = loop()
      // If needed, re-sort the table.
      // Alternatively this would also be done automatically by wrapping the
      // ObservableList into a SortedList and binding its comparator to the
      // table one: any change in the underlying list would be reflected in
      // the SortedList (automatically sorted) and visually in the TableView;
      // but since we listen to changes anyway, we can re-sort ourself without
      // having to introduce an extra SortedList.
      if (resort) {
        JFXSystem.run {
          logsTable.sort()
        }
      }
    }
    // Set initial value (only changes are listened)
    setLogs()

    // Remember this subscription
    logsTable.setUserData(logsCancellable)
  }

  private val USERDATA_HIGHLIGHTED_SITE = "highlighted-site"
  private val STYLE_HIGHLIGHT = "highlight"
  private val STYLE_HIGHLIGHTED = "highlighted"

  private def downloadRowUpdated(row: TableRow[UUID], highlightSite: Option[String]): Unit = {
    val highlight = for {
      site <- highlightSite
      data <- Option(row.getItem).flatMap(getDownloadData)
    } yield data.download.siteSettings.site == site
    Styleables.toggleStyleClass(row, STYLE_HIGHLIGHTED, set = highlight.contains(true))
  }

  private def downloadRowUpdated(row: TableRow[UUID], @unused oldValue: UUID, @unused newValue: UUID): Unit = {
    val highlightSite = Nodes.getUserDataOpt[String](downloadsTable, USERDATA_HIGHLIGHTED_SITE)
    downloadRowUpdated(row, highlightSite)
  }

  private def setSiteHighlight(highlightSite: Option[String]): Unit = {
    if (highlightSite != Nodes.getUserDataOpt[String](downloadsTable, USERDATA_HIGHLIGHTED_SITE)) {
      highlightSite match {
        case Some(site) =>
          Nodes.setUserData(downloadsTable, USERDATA_HIGHLIGHTED_SITE, site)
          // Note: prepare entries before enabling highlighting
          TableViews.getRows(downloadsTable).foreach { row =>
            downloadRowUpdated(row, highlightSite)
          }
          Styleables.toggleStyleClass(downloadsTable, STYLE_HIGHLIGHT, set = true)

        case None =>
          Styleables.toggleStyleClass(downloadsTable, STYLE_HIGHLIGHT, set = false)
          Nodes.removeUserData(downloadsTable, USERDATA_HIGHLIGHTED_SITE)
          // Note: no need to refresh rows when highlighting is off
      }
    }
    // else: no change
  }

  private def refreshDlProperties(): Unit = {
    cancelSubscription(Option(dlPropertiesTab.getUserData))

    selectedDownloadData match {
      case Some(data) =>
        val download = data.download
        val info = download.info
        val siteSettings = download.siteSettings
        val dlMngr = getState.dlMngr

        // Setup DL site highlighting
        dlSiteLink.setOnMouseEntered { _ => setSiteHighlight(Some(siteSettings.site)) }
        dlSiteLink.setOnMouseExited { _ => setSiteHighlight(None) }

        List(dlServerLink, dlServerMaxCnxField,
          dlSiteLink, dlSiteMaxCnxField, dlSiteMaxSegmentsField,
          dlURIDebugButton, dlFileSelectButton).foreach { button =>
          button.setDisable(false)
        }
        // Show (and allow changing) limits: server cnx, site cnx, site DL segments.
        // Note: reset editor text to make sure it does not remain empty. When
        // no DL is selected, we empty the text without changing the value: if
        // it is and remains 1, then the text would not be refreshed because the
        // value would actually not be changed in the spinner.
        List(dlServerMaxCnxField, dlSiteMaxCnxField, dlSiteMaxSegmentsField).foreach { field =>
          field.setValueFactory(new IntegerSpinnerValueFactory(1, Int.MaxValue))
          field.getEditor.setText(field.getValueFactory.getConverter.toString(field.getValue))
        }
        dlServerMaxCnxField.getValueFactory.setValue(Main.settings.cnxServerMax.get)
        dlServerMaxCnxField.getValueFactory.valueProperty.listen { (_, oldValue, newValue) =>
          // Change value, and try new cnx if applicable.
          Main.settings.cnxServerMax.set(newValue)
          if (newValue > oldValue) dlMngr.tryConnection()
        }
        dlSiteLink.setText(siteSettings.site)
        dlSiteMaxCnxField.getValueFactory.setValue(siteSettings.getCnxMax)
        dlSiteMaxCnxField.getValueFactory.valueProperty.listen { (_, oldValue, newValue) =>
          // Change value, and try new cnx if applicable.
          siteSettings.cnxMax.set(newValue)
          if (newValue > oldValue) dlMngr.tryConnection()
        }
        dlSiteMaxSegmentsField.getValueFactory.setValue(siteSettings.getSegmentsMax)
        dlSiteMaxSegmentsField.getValueFactory.valueProperty.listen { (_, oldValue, newValue) =>
          // Change value, and try new cnx if applicable.
          siteSettings.segmentsMax.set(newValue)
          // Also refresh downloads: segments limit changed.
          dlMngr.refreshDownloads()
          if (newValue > oldValue) dlMngr.tryConnection()
        }
        dlReferrerField.setText(download.referrer.map(_.toString).orNull)
        dlCookieField.setText(download.cookie.orNull)
        dlUserAgentField.setText(download.userAgent.orNull)
        def updateProperties(): Unit = {
          dlURIField.setText(info.actualUri.get.toString)
          dlIPField.setText(info.inetAddress.get.map(_.getHostAddress).orNull)
          dlServerLink.setText(info.actualUri.get.getHost)
          dlFolderField.setText(info.path.get.getParent.toString)
          dlFileField.setText(info.path.get.getFileName.toString)
          // Display the temporary path, if created, in a tooltip.
          if (download.created) {
            val temporaryPath = info.temporaryPath.get
            val tooltip = new Tooltip(
              // Don't display the folder if it is the actual target's one.
              if (temporaryPath.getParent != info.path.get.getParent) temporaryPath.toString
              else temporaryPath.getFileName.toString
            )
            List(dlFolderField, dlFileField).foreach { field =>
              field.setTooltip(tooltip)
            }
          }
          dlSizeLabel.setText {
            if (info.isSizeKnown) {
              val size = info.size.get
              s"$size (${Units.storage.toHumanReadable(size)})"
            } else {
              ""
            }
          }
          dlLastModifiedLabel.setText(Option(info.lastModified.get).map(dateFormatter.format).orNull)
          // Somehow changed text (e.g. dlSiteLink) is not always updated until
          // layout is triggered again (e.g. resizing). Try manual trigger.
          dlPropertiesScrollPane.layout()
        }
        dlURIDebugButton.setOnAction { _ =>
          val request = dlMngr.newRequest(
            uri = info.actualUri.get,
            head = true,
            referrer = download.referrer,
            cookie = download.cookie,
            userAgent = download.userAgent,
            rangeValidator = None,
            range = SegmentRange.zero
          )

          // Execute HEAD request, display retrieved information, and apply hints
          // if requested.
          val dialog = HeadRequestController.buildDialog(stage, dlMngr, request)
          dialog.initModality(Modality.WINDOW_MODAL)
          dialog.setResizable(true)
          dialog.showAndWait().flatten.foreach { hints =>
            hints.uri.foreach { uri =>
              info.actualUri.set(uri)
              val message = s"Actual (redirected) uri=<${info.actualUri.get}>"
              logger.info(s"${download.context} $message")
              download.info.addLog(LogKind.Info, message)
            }
            hints.size.foreach(download.setSize)
            hints.filename.filter(_ != info.path.get.getFileName.toString).foreach { filename =>
              download.renameFile(info.path.get.getParent.resolve(filename))
            }
          }
        }
        dlURIField.setOnMouseClicked { event =>
          if ((event.getButton == MouseButton.PRIMARY) && event.isControlDown && (event.getClickCount == 1)) {
            changeDlURI(download)
          }
        }
        dlFileSelectButton.setOnAction { _ =>
          val fileChooser = new FileChooser()
          fileChooser.getExtensionFilters.addAll(
            new FileChooser.ExtensionFilter("*.*", "*.*")
          )
          PathChoosers.setInitialPath(fileChooser, info.path.get.toFile)
          Option(fileChooser.showSaveDialog(stage)).foreach { selectedFile =>
            // The 'save' dialog asks whether we wish to overwrite file if
            // selected one exists. So delete the target path it it exists and
            // is not the one we use already.
            if ((selectedFile != download.downloadFile.getWorkingPath.toFile) && selectedFile.exists()) {
              selectedFile.delete()
            }
            download.renameFile(selectedFile.toPath)
          }
        }
        val propertiesCancellable = RichObservableValue.listen[Any](info.actualUri, info.path, info.inetAddress, info.size, info.lastModified) {
          JFXSystem.runLater {
            updateProperties()
          }
        }
        updateProperties()
        // Remember this subscription
        dlPropertiesTab.setUserData(propertiesCancellable)

      case None =>
        // Reset DL site highlighting
        setSiteHighlight(None)
        dlSiteLink.setOnMouseEntered(null)
        dlSiteLink.setOnMouseExited(null)

        dlURIDebugButton.setOnAction(null)
        dlURIField.setOnMouseClicked(null)
        dlFileSelectButton.setOnAction(null)
        List(dlFolderField, dlFileField).foreach { field =>
          field.setTooltip(null)
        }
        List(dlServerLink, dlServerMaxCnxField,
          dlSiteLink, dlSiteMaxCnxField, dlSiteMaxSegmentsField,
          dlURIDebugButton, dlFileSelectButton).foreach { button =>
          button.setDisable(true)
        }
        List(dlServerLink, dlSiteLink, dlSizeLabel, dlLastModifiedLabel).foreach { field =>
          field.setText(null)
        }
        // Belt and suspenders: even though the spinners are now disabled, we
        // also reset the value factory because we listen to value changes and
        // we want to be sure they won't be changed anymore.
        List(dlServerMaxCnxField, dlSiteMaxCnxField, dlSiteMaxSegmentsField).foreach { field =>
          field.setValueFactory(null)
          field.getEditor.setText(null)
        }
        List(dlURIField, dlIPField,
          dlReferrerField, dlCookieField, dlUserAgentField,
          dlFolderField, dlFileField).foreach { field =>
          field.setText(null)
        }
    }
  }

  private def changeDlURI(download: Download): Unit = {
    val originalUri = download.info.actualUri.get

    // Create simple text input dialog
    val dialog = new TextInputDialog(originalUri.toString)
    val dialogStage = Stages.getStage(dialog)
    Stages.initOwner(dialog, stage)
    dialog.setResizable(true)
    // Add simple stage location persistence
    val persistentView = StageLocationPersistentView(dialogStage, uriStageLocation)
    Dialogs.addPersistence(dialog, persistentView)
    // Load css
    Styles.addStylesheet(dialogStage.getScene)
    // We only need to display the URI value to change
    dialog.setHeaderText(null)
    dialog.setContentText(null)

    // Only accept valid URI that differs from original one.
    val field = dialog.getEditor
    val buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    def getURI: Option[URI] = try {
      Option(field.getText).filterNot(_.trim.isEmpty).map(Http.getURI)
    } catch {
      case _: Exception => None
    }
    def checkForm(): Unit = {
      val uri = getURI
      val uriOk = getURI.isDefined
      // Visual hint when URI is not valid
      Styles.toggleError(field, !uriOk, Strings.invalidURI)
      buttonOk.setDisable(!uriOk || uri.contains(originalUri))
    }
    field.textProperty.listen(checkForm())
    checkForm()

    // Allow user to change URI, and apply change when applicable
    dialog.showAndWait().foreach { _ =>
      getURI.foreach { uri =>
        download.setUri(uri)
        val message = s"Changed uri=<$uri>"
        logger.info(s"${download.context} $message")
        download.info.addLog(LogKind.Info, message)
      }
    }
  }

  private val refreshAllDlRunning: () => Unit = { () =>
    val resume = getDownloadsData.foldLeft(DownloadsResume()) {
      case (acc, data) => acc.add(data.download.state)
    }
    allDlRunningLabel.setText {
      if (resume.total == 0) ""
      else s"${
        resume.get(DownloadState.Pending)
      } + ${
        resume.get(DownloadState.Downloading)
      } + ${
        resume.get(DownloadState.Done)
      } / ${resume.total}"
    }
    allDlRunningLabel.setTooltip {
      if (resume.total == 0) null
      else {
        val text = List(
          DownloadState.Stopped,
          DownloadState.Pending,
          DownloadState.Downloading,
          DownloadState.Done,
          DownloadState.Failure
        ).foldLeft("") { (text, state) =>
          val count = resume.get(state)
          if (count > 0) s"$text\n${Strings.getState(state)}: $count"
          else text
        }
        new Tooltip(s"$text\n${Strings.total}: ${resume.total}".trim)
      }
    }
  }

  private val refreshAllDlProgress: () => Unit = { () =>
    val (downloaded, total) = getDownloadsData.foldLeft((0L, 0L)) {
      case ((downloaded0, total0), data) =>
        val info = data.download.info
        val size =
          if (info.isSizeKnown) info.size.get
          else data.download.sizeHint.getOrElse(0L)
        val downloaded =
          if (size > 0) info.downloaded.get
          else 0
        (downloaded0 + downloaded, total0 + size)
    }
    val text = if (total == 0) ""
    else {
      val sDownloaded = Units.storage.toHumanReadable(downloaded)
      val sTotal = Units.storage.toHumanReadable(total)
      val percent = downloaded * 100.0 / total
      s"%s / %s  (%.1f%%)".format(sDownloaded, sTotal, percent)
    }
    allDlProgressLabel.setText(text)
  }

  private val refreshAllDlSpeed: () => Unit = { () =>
    val rate = Units.storage.toHumanReadable(getDownloadsData.filter(_.download.isDownloading).map(_.rateHandler.currentRate).sum)
    val limit = Main.settings.rateLimitValue.get
    val sLimit = if (limit > 0) {
      s"$limit ${Main.settings.rateLimitUnit.get}/s"
    } else "∞"
    allDlSpeedLabel.setText(s"$rate/s  [$sLimit]")
  }

  /** Restores (persisted) view. */
  override protected def restoreView(): Unit = {
    super.restoreView()

    // Restore SplitPane divider positions
    splitPaneDividerPositions.opt.foreach { dividerPositions =>
      Panes.restoreDividerPositions(splitPane, dividerPositions)
    }

    // Restore columns order and width
    TableViews.setColumnsView(downloadsTable, downloadsColumns, downloadsColumnsPref.opt)
    TableViews.setColumnsView(logsTable, logsColumns, logsColumnsPref.opt)
    // By default we want to display logs in 'reverse' time order: recent logs
    // at the top of the view.
    // We don't change restored sort order and types if either:
    //  - a sort order has been set
    //  - time column sort type is not the default 'ascending' one
    // Here we rely on the fact that we do save 'ascending' as default sort
    // order, and that JavaFX does cycle on sort types in this order:
    //  - sorted, ascending
    //  - sorted, descending
    //  - unsorted, descending
    //  - sorted, ascending
    //  - ...
    // So if saved sort order is 'descending', user did change the initial
    // setup, and only the combination of 'unsorted' (empty sort order) and
    // 'ascending' time sort type means we are in the initial setup.
    if (logsTable.getSortOrder.isEmpty && (columnLogTime.getSortType == TableColumn.SortType.ASCENDING)) {
      columnLogTime.setSortType(TableColumn.SortType.DESCENDING)
      logsTable.getSortOrder.setAll(columnLogTime)
    }

    // Automatically resize 'message' column to fit the whole table width.
    TableViews.autowidthColumn(columnLogMessage)
  }

  override protected def restoreViewOnStageReady(): Unit = {
    super.restoreViewOnStageReady()

    val state = getState
    state.dlMngr.start()
    state.dlMngr.getDownloads.foreach { download =>
      addDownload(download.id, first = false, select = false)
    }
  }

  /** Persists view (stage location, ...). */
  override protected def persistView(): Unit = {
    super.persistView()

    // Persist table columns order and width
    downloadsColumnsPref.set(TableViews.getColumnsView(downloadsTable, downloadsColumns))
    logsColumnsPref.set(TableViews.getColumnsView(logsTable, logsColumns))

    // Persist SplitPane divider positions
    splitPaneDividerPositions.set(Panes.encodeDividerPositions(splitPane))
  }

  private def getState: State = {
    stage.getUserData.asInstanceOf[State]
  }

  private def selectedDownload: Option[UUID] = Option(downloadsTable.getSelectionModel.getSelectedItem)
  private def selectedDownloadData: Option[DownloadData] = selectedDownload.flatMap(getDownloadData)

  private def selectedDownloadsIdx: List[Int] = downloadsTable.getSelectionModel.getSelectedIndices.asScala.toList.map(Int.unbox)
  private def selectedDownloads: List[UUID] = downloadsTable.getSelectionModel.getSelectedItems.asScala.toList
  private def selectedDownloadsData: List[DownloadData] = selectedDownloads.flatMap(getDownloadData)

  private def getDownloadsData: List[DownloadData] = downloadsTable.getItems.asScala.toList.flatMap(getDownloadData)

  private def getDownloadData(id: UUID): Option[DownloadData] = this.synchronized {
    downloadData.get(id)
  }

  private def setDownloadData(id: UUID, data: DownloadData): Unit = this.synchronized {
    downloadData += (id -> data)
  }

  private def removeDownloadData(id: UUID): Unit = this.synchronized {
    downloadData -= id
  }

  private def enableMenuStop(menu: MenuItem, data: List[DownloadData]): Unit = {
    menu.setDisable(!data.exists(_.download.canStop))
  }

  private def enableMenuResume(menu: MenuItem, data: List[DownloadData]): Unit = {
    menu.setDisable(!data.exists(_.download.canResume))
  }

  private def enableMenuRemoveCompleted(menu: MenuItem, data: List[DownloadData]): Unit = {
    menu.setDisable(!data.exists(d => d.download.isDone && d.download.doneError.isEmpty))
  }

  private def enableMenuRemove(menu: MenuItem, data: List[DownloadData]): Unit = {
    menu.setDisable(data.isEmpty)
  }

  private val downloadsContextMenu: ContextMenu = {
    val contextMenu = new ContextMenu()

    val stopDownload = new MenuItem(Strings.stop)
    stopDownload.setGraphic(Icons.stop().pane)
    stopDownload.setOnAction { _ =>
      selectedDownloadsData.filter(_.download.canStop).foreach { data =>
        getState.dlMngr.stopDownload(data.download.id)
      }
    }
    val resumeDownload = new MenuItem(Strings.resume)
    resumeDownload.setGraphic(Icons.download().pane)
    resumeDownload.setOnAction { _ =>
      selectedDownloadsData.filter(_.download.canResume(restart = false)).foreach { data =>
        getState.dlMngr.resumeDownload(data.download.id, restart = false, tryCnx = false)
      }
      getState.dlMngr.tryConnection()
    }
    val restartDownload = new MenuItem(Strings.restart)
    restartDownload.setGraphic(Icons.undo().pane)
    restartDownload.setOnAction { _ =>
      selectedDownloadsData.filter(_.download.canResume(restart = true)).foreach { data =>
        getState.dlMngr.resumeDownload(data.download.id, restart = true, tryCnx = false)
      }
      getState.dlMngr.tryConnection()
    }

    val stopAll = new MenuItem(Strings.stopAll)
    stopAll.setGraphic(Icons.stop().pane)
    stopAll.setOnAction(onDownloadsStopAll)
    val resumeAll = new MenuItem(Strings.resumeAll)
    resumeAll.setGraphic(Icons.download().pane)
    resumeAll.setOnAction(onDownloadsResumeAll)
    val removeCompleted = new MenuItem(Strings.removeCompleted)
    removeCompleted.setGraphic(Icons.eraser().pane)
    removeCompleted.setOnAction(onDownloadsRemoveCompleted)
    val remove = new MenuItem(Strings.remove)
    remove.setGraphic(Icons.minus().pane)
    remove.setOnAction(onDownloadsRemove)

    val moveFirst = new MenuItem(Strings.moveFirst)
    moveFirst.setGraphic(Icons.angleDoubleUp().pane)
    moveFirst.setOnAction { _ =>
      moveDownloads(selectedDownloads, up = true, most = true)
    }
    val moveUp = new MenuItem(Strings.moveUp)
    moveUp.setGraphic(Icons.angleUp().pane)
    moveUp.setOnAction { _ =>
      moveDownloads(selectedDownloads, up = true, most = false)
    }
    val moveDown = new MenuItem(Strings.moveDown)
    moveDown.setGraphic(Icons.angleDown().pane)
    moveDown.setOnAction { _ =>
      moveDownloads(selectedDownloads, up = false, most = false)
    }
    val moveLast = new MenuItem(Strings.moveLast)
    moveLast.setGraphic(Icons.angleDoubleDown().pane)
    moveLast.setOnAction { _ =>
      moveDownloads(selectedDownloads, up = false, most = true)
    }

    contextMenu.getItems.addAll(
      stopDownload, resumeDownload, restartDownload,
      new SeparatorMenuItem,
      stopAll, resumeAll, removeCompleted, remove,
      new SeparatorMenuItem,
      moveFirst, moveUp, moveDown, moveLast
    )
    contextMenu.setOnShowing { _ =>
      val _data = getDownloadsData
      val _selectedIndices = selectedDownloadsIdx
      val _selectedData = selectedDownloadsData
      enableMenuStop(stopDownload, _selectedData)
      enableMenuResume(resumeDownload, _selectedData)
      restartDownload.setDisable(!_selectedData.exists(_.download.canRestart))
      enableMenuStop(stopAll, _data)
      enableMenuResume(resumeAll, _data)
      enableMenuRemoveCompleted(removeCompleted, _data)
      enableMenuRemove(remove, _data)
      val items = downloadsTable.getItems
      // If all items are in first positions, we cannot move them up
      val cannotMoveUp = _selectedIndices.isEmpty || (_selectedIndices.max + 1 == _selectedIndices.size)
      // If all items are in last positions, we cannot move them down
      val cannotMoveDown = _selectedIndices.isEmpty || (items.size - _selectedIndices.min == _selectedIndices.size)
      moveFirst.setDisable(cannotMoveUp)
      moveUp.setDisable(cannotMoveUp)
      moveDown.setDisable(cannotMoveDown)
      moveLast.setDisable(cannotMoveDown)
    }

    contextMenu
  }

  def displayError(title: Option[String], contentText: Option[String], ex: Throwable): Unit = {
    // Note: caller does not log this error, do it here.
    val msg = s"Caught error:${
      title.map(v => s" title=<$v>").getOrElse("")
    }${
      contentText.map(v => s" text=<$v>").getOrElse("")
    } ${ex.getMessage}"
    logger.error(msg, ex)
    addLog(LogKind.Error, msg, Some(ex))
    Dialogs.error(
      owner = Some(stage),
      title = title.orElse(Some(stage.getTitle)),
      contentText = contentText.orElse(Some(Strings.unexpectedIssue)),
      ex = Some(ex)
    )
    ()
  }

  /**
   * Asks confirmation to allow by-passing SSL trusting error.
   *
   * @param site concerned site
   * @param host concerned host
   * @return whether a new attempt can be made (SSL will be trusted)
   */
  def askOnSslError(site: String, host: String, ex: Exception): Future[Boolean] = {
    val siteSettings = Main.settings.getSite(site)
    val canTrustSite = !siteSettings.isDefault
    val buttonSite = new ButtonType(Strings.site)
    val buttonServer = new ButtonType(Strings.server)
    val buttonNo = ButtonType.NO

    val b1 = if (canTrustSite) Some(buttonSite) else None
    val buttons = b1.toList ::: List(buttonServer, buttonNo)
    val defaultButton = b1.orElse(Some(buttonServer))
    // Don't block the current thread.
    RichFuture.blockingAsync {
      // Prevent more than one dialog at a time for a given site. Useful if we
      // have more than one download for the concerned site/host, in which case
      // we can re-check settings before asking user (in case trusting was set
      // in a previous confirmation).
      siteSettings.synchronized {
        val dlMngr = getState.dlMngr
        val serverConnections = dlMngr.getServerConnections(site, host)
        if (serverConnections.sslErrorAsk.contains(true)) {
          // We need to ask confirmation.
          val text =
            s"""${Strings.sslIssue}
               |${Strings.site}: $site
               |${Strings.server}: $host""".stripMargin
          Dialogs.confirmation(
            owner = Option(stage),
            title = None,
            headerText = Some(text),
            contentText = Some(Strings.sslTrust),
            ex = Some(ex),
            buttons = buttons,
            defaultButton = defaultButton
          ).getOrElse(ButtonType.CANCEL) match {
            case `buttonSite` =>
              dlMngr.trustSslSiteConnection(site, trust = true)
              siteSettings.sslTrust.set(true)
              true

            case `buttonServer` =>
              dlMngr.trustSslServerConnection(site, host, trust = true)
              true

            case `buttonNo` =>
              dlMngr.trustSslServerConnection(site, host, trust = false)
              false

            case _ =>
              false
          }
        } else {
          serverConnections.sslTrust || serverConnections.sslErrorAsk.isEmpty
        }
      }
    }
  }

  def addDownload(dlParams: Main.Params): Future[Unit] = {
    val promise = Promise[Unit]()
    actor ! OnDownloadsAdd(dlParams, promise)
    promise.future
  }

  def onCloseRequest(event: WindowEvent): Unit = {
    try {
      actor ! OnExit
    } catch {
      case ex: NoClassDefFoundError =>
        // Usually happens when jar has been replaced (on Windows at least).
        // There is nothing much to do except exit.
        System.err.println("Exiting because classes are missing (JAR may have been overwritten)!")
        ex.printStackTrace()
        sys.exit(1)
    }
    // Note: consume the event, the actor is responsible for shutting down
    event.consume()
  }

  @unused
  def onOptions(@unused event: ActionEvent): Unit = {
    actor ! OnOptions()
  }

  @unused
  def onAbout(@unused event: ActionEvent): Unit = {
    actor ! OnAbout
  }

  @unused
  def onExit(@unused event: ActionEvent): Unit = {
    actor ! OnExit
  }

  @unused
  def onDownloadsAdd(@unused event: ActionEvent): Unit = {
    addDownload(Main.Params())
    ()
  }

  def onDownloadsStopAll(@unused event: ActionEvent): Unit = {
    getDownloadsData.filter(_.download.canStop).foreach { data =>
      getState.dlMngr.stopDownload(data.download.id)
    }
  }

  def onDownloadsResumeAll(@unused event: ActionEvent): Unit = {
    getDownloadsData.filter(_.download.canResume(restart = false)).foreach { data =>
      getState.dlMngr.resumeDownload(data.download.id, restart = false, tryCnx = false)
    }
    getState.dlMngr.tryConnection()
  }

  def onDownloadsRemoveCompleted(@unused event: ActionEvent): Unit = {
    actor ! OnDownloadsRemoveCompleted
  }

  def onDownloadsRemove(@unused event: ActionEvent): Unit = {
    onDownloadsRemove(force = false)
  }

  private def onDownloadsRemove(force: Boolean): Unit = {
    actor ! OnDownloadsRemove(force)
  }

  @unused
  def onDlServerSettings(@unused event: ActionEvent): Unit = {
    selectedDownloadData.foreach { data =>
      val host = data.download.info.actualUri.get.getHost
      actor ! OnOptions(OptionsController.Display(serverSettings = Some(host)))
    }
  }

  @unused
  def onDlSiteSettings(@unused event: ActionEvent): Unit = {
    selectedDownloadData.foreach { data =>
      val siteSettings = data.download.siteSettings
      actor ! OnOptions(OptionsController.Display(siteSettings = Some(siteSettings)))
    }
  }

  def addDownload(id: UUID, first: Boolean, select: Boolean): Unit = {
    actor ! AddDownload(id, first, select)
  }

  private def moveDownloads(ids: List[UUID], up: Boolean, most: Boolean): Unit = {
    actor ! MoveDownloads(ids, up, most)
  }

  private class ControllerActor(state0: State) extends Actor {

    override def receive: Receive = receive(state0)

    // Wrap actual partial function in a try/catch to display unexpected issues.
    // Otherwise, window becomes unusable (akka messages goes to dead letters).
    private def receive(state: State): Receive =
      new PartialFunction[Any, Unit]() {
        val r: Receive = receive0(state)
        override def isDefinedAt(x: Any): Boolean = r.isDefinedAt(x)
        override def apply(x: Any): Unit = try {
          r.apply(x)
        } catch {
          case ex: Exception =>
            displayError(
              title = Some(state.stage.getTitle),
              contentText = Some(Strings.unexpectedIssue),
              ex = ex
            )
            ()
        }
      }

    private def receive0(state: State): Receive = {
      case OnOptions(display)                => onOptions(state, display)
      case OnAbout                           => onAbout(state)
      case OnExit                            => onExit(state)
      case OnDownloadsAdd(dlParams, promise) => onDownloadsAdd(state, dlParams, promise)
      case OnDownloadsRemoveCompleted        => onDownloadsRemoveCompleted(state)
      case OnDownloadsRemove(force)          => onDownloadsRemove(state, force)
      case AddDownload(id, first, select)    => addDownload(state, id, first, select)
      case MoveDownloads(ids, up, most)      => moveDownloads(state, ids, up, most)
    }

    private def onExit(state: State): Unit = {
      val canExit = !getDownloadsData.exists(_.download.canStop) || {
        Dialogs.confirmation(
          owner = Option(stage),
          title = None,
          headerText = Some(Strings.stopDlsOnExit),
          contentText = None,
          buttons = List(ButtonType.OK, ButtonType.CANCEL),
          defaultButton = Some(ButtonType.OK)
        ).getOrElse(ButtonType.CANCEL) match {
          case ButtonType.OK => true
          case _ => false
        }
      }
      if (canExit) {
        persistView()

        state.dlMngr.stop().onComplete { _ =>
          val ok = try {
            state.dlMngr.saveState()
            true
          } catch {
            case ex: Exception =>
              displayError(
                title = None,
                contentText = Some(s"${Strings.writeIssue}\n${Main.statePath}"),
                ex = ex
              )
              false
          }
          if (ok) {
            context.stop(self)
            Main.shutdown(state.stage)
          }
        }(Main.Akka.dispatcher)
      }
    }

    private def onOptions(state: State, display: OptionsController.Display): Unit = {
      val dlMngr = state.dlMngr
      val debug0 = Main.settings.debug.get
      val dialog = OptionsController.buildDialog(state.stage, display)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.setResizable(true)
      val result = dialog.showAndWait().orElse(OptionsController.Result())

      // Refresh selected DL logs to show/hide debug events.
      if (Main.settings.debug.get != debug0) {
        refreshLogs()
      }

      // If SSL trusting changed, refresh download manager connections.
      if (result.sslTrustChanged) {
        dlMngr.refreshConnections()
      }

      // If sites or limits were changed, refresh downloads: in particular the
      // maximum number of segments will be recomputed, and the current
      // connections associated to the correct site.
      // Also refresh properties: selected download site may have changed, as
      // well as site limits.
      if (result.sitesChanged || result.cnxLimitChanged) {
        dlMngr.refreshDownloads()
        refreshDlProperties()
      }

      // If cnx limits were (or may have) changed, ping downloads.
      // We only need to do it once: all downloads will be pinged (in order)
      // and will acquire new connection(s) when possible.
      if (result.cnxLimitChanged) {
        dlMngr.tryConnection()
      }
      // If cnx buffer was changed, re-create the HTTP client.
      if (result.cnxBufferChanged) {
        dlMngr.setClient()
      }

      // Reload controller if needed (language change).
      if (result.reload) {
        // Reset I18N cache to apply any language change
        I18N.reset()
        // Persist now to restore it when rebuilding the stage
        persistView()
        context.stop(self)
        MainController.build(state)
        ()
      }
    }

    private def onAbout(state: State): Unit = {
      val dialog = AboutController.buildDialog(state.stage)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.setResizable(true)
      dialog.show()
    }

    private def onDownloadsAdd(state: State, dlParams: Main.Params, promise: Promise[Unit]): Unit = {
      // We actually notify caller once we take into account the new download,
      // not whether we fail, user discard it, nor when it actually is added.
      // So complete the promise now, in case we fail to build the dialog, in
      // which case an appropriate error will be displayed.
      promise.trySuccess(())
      val dialogOpt = NewDownloadController.buildDialog(MainController.this, state.stage, state.dlMngr, dlParams)
      // In automatic mode, there is no dialog and download has been added.
      dialogOpt.foreach { dialog =>
        dialog.initModality(Modality.WINDOW_MODAL)
        dialog.setResizable(true)
        // On Windows (10), a modal dialog can be shown while the parent window
        // remain iconified.
        // On Gnome (3.34), showing a modal dialog does de-iconify the parent
        // window, which is usually annoying as it remains there after closing
        // the dialog.
        // Setting the dialog without parent but "application modal" prevents
        // the main application window to appear but only when showing the
        // dialog: once closed the main application window is de-iconified.
        // As a workaround, we can re-iconify the main stage once the dialog
        // is closed.
        if (stage.isIconified) Stages.onStageClosed(Stages.getStage(dialog)) {
          // We need to defer iconification for it to properly work.
          JFXSystem.runLater {
            stage.setIconified(true)
          }
        }
        dialog.show()
      }
      ()
    }

    private def onDownloadsRemoveCompleted(state: State): Unit = {
      getDownloadsData.foreach { data =>
        if (data.download.isDone && data.download.doneError.isEmpty) {
          removeDownload(state, data.download.id)
        }
      }
    }

    private def onDownloadsRemove(state: State, force: Boolean): Unit = {
      val selected = selectedDownloadsData

      def doRemove(): Unit = {
        // We don't remove downloads that needs to be stopped.
        val removable = selected.filterNot(_.download.canStop)

        def removeSubtitle(download: Download): Unit = {
          // Delete subtitle file if applicable.
          download.info.subtitle.flatMap(_.filename).foreach { subtitleFilename =>
            // Subtitle file is saved next to target (not temporary)
            // download file.
            download.path.resolveSibling(subtitleFilename).toFile.delete()
          }
        }

        // We can safely remove if done (success) or not started (failure or not)
        val safe = removable.filter { data =>
          (data.download.isDone && data.download.doneError.isEmpty) || !data.download.isStarted
        }
        safe.foreach { data =>
          val download = data.download
          removeDownload(state, download.id)
          // We obviously can only delete subtitles if download was not done
          // (either not started, or completed but considered invalid).
          if (!data.download.isDone) removeSubtitle(download)
        }

        // Get remaining selected downloads (started and unfinished)
        val unfinished = removable.filterNot(safe.contains)
        if (unfinished.nonEmpty) {
          val confirmation = if (!force) {
            val dialog = RemoveUnfinishedController.buildDialog(stage, unfinished)
            dialog.showAndWait().flatten
          } else {
            Some(true)
          }

          // Apply action
          confirmation.foreach { removeFromDisk =>
            unfinished.foreach { data =>
              val download = data.download
              // Remove download entry
              removeDownload(state, download.id)
              // Remove from disk when requested
              if (removeFromDisk) {
                // Since the download is unfinished, we only need to delete the
                // temporary file if any (or the real file otherwise).
                download.downloadFile.getWorkingPath.toFile.delete()
                removeSubtitle(download)
              }
            }
          }
        }
      }

      // Stop downloads that need it. Otherwise, we can remove now.
      val needStop = selected.filter(_.download.canStop)
      val removeNow = needStop.isEmpty || {
        // Ask for confirmation if necessary.
        val stop = force || Dialogs.confirmation(
          owner = Some(stage),
          title = None,
          headerText = Some(Strings.stopDlsOnRemove),
          contentText = Some(needStop.map(_.path.get.getFileName).mkString("\n")),
          buttons = List(ButtonType.OK, ButtonType.CANCEL),
          defaultButton = Some(ButtonType.OK)
        ).contains(ButtonType.OK)

        if (stop) {
          import context.dispatcher
          import RichFuture._

          // Wait for downloads to be stopped before removing them, and fail
          // if it takes too long.
          // Report and don't do anything if there was a failure.
          Future.sequence {
            needStop.flatMap { dlEntry =>
              state.dlMngr.stopDownload(dlEntry.download.id)
            }
          }.map(_ => ()).withTimeout(Settings.DL_STOP_TIMEOUT).onComplete {
            case Success(_) =>
              doRemove()

            case Failure(ex) =>
              displayError(
                title = None,
                contentText = None,
                ex = ex
              )
          }
          false
        } else {
          // Stopping was cancelled, we only need to remove what we can.
          true
        }
      }
      if (removeNow) doRemove()
    }

    private def removeDownload(state: State, id: UUID): Unit = {
      // Remove this download from the manager, our own data, and the table items
      state.dlMngr.removeDownload(id)
      removeDownloadData(id)
      downloadsTable.getItems.remove(id)
      ()
    }

    private def addDownload(state: State, id: UUID, first: Boolean, select: Boolean): Unit = {
      val items = downloadsTable.getItems
      if (!items.asScala.toSet.contains(id)) {
        state.dlMngr.getDownload(id).foreach { download =>
          val info = download.info
          val data = new DownloadData(download)
          setDownloadData(id, data)

          BindingsEx.jfxBind(data.path, info.path) {
            info.path.get
          }

          def displaySize: String = {
            val (size, qualifier) =
              if (info.isSizeKnown) (Option(info.size.get), None)
              else if (download.isDone) (Some(info.downloaded.get), None)
              else (download.sizeHint, download.sizeQualifier)
            size.filter(_ >= 0).map { size =>
              s"${qualifier.getOrElse("")}${Units.storage.toHumanReadable(size)}"
            }.orNull
          }
          // Display size right now (if known or hint given)
          data.size.set(displaySize)
          // Then refresh when actual size is determined
          new BindingsEx.Builder(JFXSystem.scheduler).add(data.size) {
            displaySize
          }.add(data.sizeIcon) {
            val warnings =
              (if (!info.isSizeUnknown) Nil else List(Strings.unknownSize)) :::
                (if (!download.acceptRanges.contains(false)) Nil else List(Strings.resumeUnsupported)) :::
                download.sizeHint.filter { hint =>
                  // When there is a qualifier, the hint size usually will not
                  // match the actual size, and this is normal.
                  download.isDone && (hint != info.downloaded.get) && download.sizeQualifier.isEmpty
                }.map { hint =>
                  Strings.hintSizeMismatch.format(info.downloaded.get, hint)
                }.toList

            if (warnings.nonEmpty) {
              val icon = Icons.exclamationTriangle(styleClass = List("icon-exclamation-triangle-warning")).pane
              val tooltip = new Tooltip(warnings.mkString("\n"))
              Tooltip.install(icon, tooltip)
              icon
            } else null
          }.sideEffect(refreshAllDlProgress)
           .bind(info.size, info.acceptRanges, info.state)

          BindingsEx.bind(data.downloadedProgress.progressProperty, throttlingFast, jfxThrottler, info.downloaded, info.state) {
            Option(info.size.get).filter(_ >= 0).map { size =>
              // Limit precision to 3 digits (1/10th of percent), so that actual
              // progress is not updated more than 1000 times (which limits CPU
              // usage even if we are called very frequently).
              if (size > 0) BigDecimal(info.downloaded.getValue.toDouble / size).setScale(3, RoundingMode.DOWN).doubleValue
              else 0.0
            }.getOrElse {
              // Note: Usually we would set progress to -1.0 if isSizeDetermined
              // when running. But the "indeterminate state" animation consumes
              // more CPU (and apparently too much under Linux).
              info.state.get match {
                case DownloadState.Done => 1.0
                case _ => 0.0
              }
            }:Double
          }
          new BindingsEx.Builder(jfxThrottler).add(data.downloadedProgressText.textProperty) {
            val downloaded = info.downloaded.get
            val size = info.size.get
            if (downloaded <= 0) null
            else if (size > 0) {
              val percent = downloaded * 100.0 / size
              "%.1f%% [%s]".format(percent, Units.storage.toHumanReadable(downloaded))
            } else {
              Units.storage.toHumanReadable(downloaded)
            }
          }.sideEffect(refreshAllDlProgress)
           .bind(throttlingFast, info.downloaded)

          // Notes:
          // Rate value is computed often (depends on rate handler time slice),
          // but displayed less often.
          new BindingsEx.Builder(jfxThrottler).add(data.rate) {
            if (download.isDownloading) {
              val rate = data.rateValue.get
              s"${Units.storage.toHumanReadable(rate)}/s"
            } else {
              null
            }
          }.add(data.eta) {
            if (download.isDownloading) {
              Option(info.size.get).filter(_ > 0).map { size =>
                val remaining = size - info.downloaded.get
                val rate = data.rateValue.get
                if (rate > 0) {
                  val seconds = (remaining.toDouble / rate).ceil.toLong
                  s"%02d:%02d:%02d".format(seconds / (60 * 60), (seconds / 60) % 60, seconds % 60)
                } else "∞"
              }.orNull
            } else {
              null
            }
          }.sideEffect(refreshAllDlSpeed)
           .bind(throttlingSlow, data.rateValue, info.downloaded, info.state)

          BindingsEx.jfxBind(data.stateIcon, info.state, info.activeSegments) {
            // @nowarn workarounds scala 2.13.x false-positive
            (download.state: @nowarn) match {
              case DownloadState.Pending =>
                Icons.hourglass().pane

              case DownloadState.Downloading =>
                val styleClass = if (download.activeSegments == 0) List("icon-download-started") else List("icon-download-running")
                Icons.download(styleClass = styleClass).pane

              case DownloadState.Done =>
                if (info.doneError.isEmpty) {
                  if (Main.settings.removeCompleted.get) removeDownload(state, data.download.id)
                  Icons.checkSquare().pane
                } else {
                  // Note: the parent table cell has a tooltip, preventing any
                  // sub-node (like this icon) tooltip to be use-able.
                  Icons.squareXmark().pane
                }

              case DownloadState.Stopped =>
                Icons.stop().pane

              case DownloadState.Failure =>
                Icons.exclamationTriangle().pane
            }
          }
          BindingsEx.jfxBind(data.segments, info.state, info.segments, info.maxSegments) {
            if (download.isDownloading) {
              s"${info.segments.get}/${info.maxSegments.get}"
            } else {
              null
            }
          }
          info.state.listen {
            JFXSystem.runLater {
              refreshAllDlRunning()
            }
          }

          if (first) items.add(0, id)
          else items.add(id)
          // Belt and suspenders: re-order downloads in manager.
          // Usually this should not be necessary. But it may happen during
          // race condition: download created in manager and a reordering
          // happened here (entry moved) before adding was received; in this
          // case, the download is now last in the manager (as not known in
          // the ordered list that was used).
          state.dlMngr.reorderDownloads(items.asScala.toList)
          if (select) {
            downloadsTable.getSelectionModel.clearSelection()
            downloadsTable.getSelectionModel.select(id)
          }
        }
        // else: unknown download, nothing much we can do
      }
      // else: already listed (should not happen)
      ()
    }

    /**
     * Move downloads up or down.
     *
     * Listed downloads are expected to be in ascending index order.
     */
    private def moveDownloads(state: State, ids: List[UUID], up: Boolean, most: Boolean): Unit = {
      // Save current selection
      val selectedItem = downloadsTable.getSelectionModel.getSelectedItem
      val selectedItems = selectedDownloads.toSet

      val items = downloadsTable.getItems
      // Remember current anchor (which is expected to be set since to move rows
      // they must have been selected, which sets an anchor).
      val anchored = Option(CellBehaviorBase.getAnchor[TablePosition[UUID, Any]](downloadsTable, null)).map { pos =>
        items.get(pos.getRow)
      }

      // Scroll when necessary: we want to keep the selected items (at least the
      // 'leading' one - first or last depending if we move up or down) visible.
      // Do it before altering items order to prevent any glitch (updated row
      // may not be properly rendered right after scrolling to top/bottom).
      val idx = if (up) {
        if (most) Int.MinValue else items.indexOf(ids.head) - 1
      } else {
        if (most) Int.MaxValue else items.indexOf(ids.last) + 1
      }
      TableViews.scrollTo(downloadsTable, idx, top = up, padding = 1)

      def moveDownload(id: UUID, idx: Int): Unit = {
        val idxSrc = items.indexOf(id)
        if (idx != idxSrc) {
          if (math.abs(idx - idxSrc) == 1) {
            // Swap item with previous/next one
            items.set(idxSrc, items.get(idx))
            items.set(idx, id)
            ()
          } else {
            // Remove item to re-add it in position
            items.remove(idxSrc)
            items.add(idx, id)
          }
        }
      }

      // Clear selection before moving item
      downloadsTable.getSelectionModel.clearSelection()
      if (up) {
        // When moving up, use the first item as base.
        val dst = if (most) 0 else math.max(0, items.indexOf(ids.head) - 1)
        ids.zipWithIndex.foreach {
          case (id, offset) => moveDownload(id, dst + offset)
        }
      } else {
        // When moving down, use the last item as base.
        val idxMax = items.size - 1
        val dst = if (most) idxMax else math.min(idxMax, items.indexOf(ids.last) + 1)
        // Since we are moving each item downward - from first to last -, they
        // individually all go to the same position so that at the end the last
        // one is really moved to the target position while others are right
        // before it (and remain in original order).
        ids.foreach { id =>
          moveDownload(id, dst)
        }
      }

      // Re-order downloads in manager
      state.dlMngr.reorderDownloads(items.asScala.toList)

      // Restore selected items: the last one to be selected will be the one
      // returned by one-item getters.
      (selectedItems - selectedItem).foreach(downloadsTable.getSelectionModel.select)
      downloadsTable.getSelectionModel.select(selectedItem)

      // Restore the selection anchor when applicable. If we don't, since we
      // changed the rows, using shift to extend selection may select more or
      // less than expected (from the original anchor to the new focused row).
      anchored.foreach { id =>
        val items = downloadsTable.getItems
        downloadsTable.getSelectionModel.getSelectedCells.asScala.find { pos =>
          items.get(pos.getRow) == id
        }.foreach { pos =>
          CellBehaviorBase.setAnchor(downloadsTable, pos, false)
        }
      }
    }

  }

}

object MainController {

  val dateFormatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z")

  case class State(
    stage: Stage,
    dlMngr: DownloadManager
  ) {
    def save(): Unit = stage.setUserData(this)
  }

  class DownloadData(var download: Download) {
    private val info = download.info

    // Notes:
    // In the downloads table, we may wish to bind some Cell properties to a
    // download. Since the cell item may change, we must be able to 'reset' the
    // binding easily.
    // As most - if not all - download properties are not updated within JavaFX
    // we need to go through BindingsEx (no real bindings, but Cancellables).
    // The easiest way is then to create an intermediate Property here, which is
    // updated though BindingsEx (created when download is added) in JavaFX
    // thread and use a normal Binding in the table cells (with which we can
    // simply 'unbind').
    val path: SimpleObjectProperty[Path] = new SimpleObjectProperty()
    val size: SimpleStringProperty = new SimpleStringProperty()
    val sizeIcon: SimpleObjectProperty[Pane] = new SimpleObjectProperty()
    val segments: SimpleStringProperty = new SimpleStringProperty()
    val stateIcon: SimpleObjectProperty[Pane] = new SimpleObjectProperty()
    val rate: SimpleStringProperty = new SimpleStringProperty()
    val eta: SimpleStringProperty = new SimpleStringProperty()

    // Fake value to listen on which regularly triggers rate update.
    // While download is running, regularly sets the value which is resetted
    // when rate is changed: while data are downloaded, this changes nothing,
    // but if download is still running but frozen, this forces rate update.
    private val rateUpdate: SimpleBooleanProperty = new SimpleBooleanProperty()
    private var rateUpdateCancellable: Option[Cancelable] = None

    private def refreshRateUpdate(state: DownloadState.Value): Unit = this.synchronized {
      val running = state == DownloadState.Downloading
      rateUpdateCancellable match {
        case Some(cancellable) =>
          if (!running) {
            cancellable.cancel()
            rateUpdateCancellable = None
          }

        case None =>
          if (running) {
            rateUpdateCancellable = Some(Main.scheduler.scheduleWithFixedDelay(throttlingSlow, throttlingSlow) {
              // Inverting the current value is a little trick that validates
              // the observable ('get') and immediately invalidates it ('set'
              // with different value). With this, any listener is guaranteed
              // to be notified right now.
              rateUpdate.set(!rateUpdate.get)
            })
          }
      }
    }

    info.state.listen(refreshRateUpdate _)
    refreshRateUpdate(download.state)

    val rateHandler = new RateHandler(download.rateLimiter, download.info.downloaded.get, 4.seconds)
    val rateValue = new SimpleLongProperty()
    BindingsEx.bind(rateValue, rateHandler.timeSlice, Main.scheduler, info.downloaded, info.state, rateUpdate) {
      if (download.isDownloading) {
        rateHandler.update(info.downloaded.get)
      } else {
        0L
      }
    }

    val downloadedProgress: ProgressBar = new ProgressBar()
    downloadedProgress.setProgress(0.0)
    downloadedProgress.setMaxWidth(Double.MaxValue)
    downloadedProgress.setMaxHeight(Double.MaxValue)
    downloadedProgress.setMouseTransparent(true)
    val downloadedProgressText = new Text()
    downloadedProgressText.getStyleClass.add("text")

    val downloadedProgressPane = new StackPane()
    downloadedProgressPane.setMaxWidth(Double.MaxValue)
    downloadedProgressPane.setMaxHeight(Double.MaxValue)
    downloadedProgressPane.getChildren.setAll(downloadedProgress, downloadedProgressText)
  }

  private case class DownloadsResume(total: Int = 0, states: Map[DownloadState.Value, Int] = Map.empty) {
    def add(state: DownloadState.Value): DownloadsResume =
      copy(total = total + 1, states = states + (state -> (get(state) + 1)))
    def get(state: DownloadState.Value): Int = states.getOrElse(state, 0)
  }

  case class OnOptions(display: OptionsController.Display = OptionsController.Display())
  case object OnAbout
  case object OnExit
  case class OnDownloadsAdd(dlParams: Main.Params, promise: Promise[Unit])
  case object OnDownloadsRemoveCompleted
  case class OnDownloadsRemove(force: Boolean)
  case class AddDownload(id: UUID, first: Boolean, select: Boolean)
  case class MoveDownloads(ids: List[UUID], up: Boolean, most: Boolean)

  // 'fast' throttling (for downloaded size, ...)
  private val throttlingFast = 500.millis

  // 'slow' throttling (for rate/ETA, ...)
  private val throttlingSlow = 1.second

  private val settingsKeyPrefix = "main"

  private val stageLocation = ConfigEntry.from[StageLocation](Main.settings.settings,
    Settings.prefix ++ Seq(Settings.KEY_STAGE, settingsKeyPrefix, Settings.KEY_LOCATION))

  private val uriStageLocation = ConfigEntry.from[StageLocation](Main.settings.settings,
    Settings.prefix ++ Seq(Settings.KEY_STAGE, s"$settingsKeyPrefix-uri", Settings.KEY_LOCATION))

  private val splitPaneDividerPositions: ConfigEntry[String] = ConfigEntry.from[String](Main.settings.settings,
    Settings.prefix ++ Seq(Settings.KEY_STAGE, settingsKeyPrefix, "splitPane", "dividerPositions"))

  private val downloadsColumnsPref: ConfigEntry[String] = ConfigEntry.from[String](Main.settings.settings,
    Settings.prefix ++ Seq(Settings.KEY_STAGE, settingsKeyPrefix, "downloads", "columns"))

  private val logsColumnsPref: ConfigEntry[String] = ConfigEntry.from[String](Main.settings.settings,
    Settings.prefix ++ Seq(Settings.KEY_STAGE, settingsKeyPrefix, "logs", "columns"))

  def build(state: State): Unit = {
    val stage = state.stage
    val first = Option(stage.getScene).isEmpty
    // Set stage icons.
    // The bare minimum is 32x32 (fits well when resized to 16x16).
    List(256.0, 128.0, 64.0, 32.0, 16.0).foreach { size =>
      val icon = Icons.download(targetSvgSize = size)
      // We want to apply CSS, but for it to work properly there must be a
      // "root" element (which holds some JavaFX CSS variables).
      icon.pane.getStyleClass.add("root")
      icon.pane.getStylesheets.add(getClass.getResource("/css/main.css").toExternalForm)
      stage.getIcons.add(Graphics.buildImage(icon.pane))
    }

    val loader = new FXMLLoader(getClass.getResource("/fxml/main.fxml"), I18N.getResources)
    val root = loader.load[Parent]()
    Main.controller = loader.getController[MainController]

    if (!first) stage.hide()
    // Delegate closing request to controller
    stage.setOnCloseRequest(Main.controller.onCloseRequest _)
    stage.setScene(new Scene(root))
    SplitPaneSkinEx.addStylesheet(stage.getScene)
    Graphics.addStylesheet(stage.getScene)
    Styles.addStylesheet(stage.getScene)
    stage.getScene.getStylesheets.add(getClass.getResource("/css/main.css").toExternalForm)

    // Initialize controller *after* setting stage scene
    Main.controller.initialize(state)

    Stages.addPersistence(stage, Main.controller, persist = false)
    stage.show()
  }

}
