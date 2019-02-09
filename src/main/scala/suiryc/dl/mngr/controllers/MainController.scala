package suiryc.dl.mngr.controllers

import akka.actor.{Actor, ActorRef, Props}
import com.sun.javafx.scene.control.VirtualScrollBar
import com.sun.javafx.tk.Toolkit
import com.typesafe.scalalogging.StrictLogging
import java.io.{PrintWriter, StringWriter}
import java.nio.file.Path
import java.time.format.DateTimeFormatter
import java.util.UUID
import javafx.beans.property.{SimpleBooleanProperty, SimpleObjectProperty, SimpleStringProperty}
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Node, Parent, Scene}
import javafx.scene.control._
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import javafx.scene.layout.{Pane, Region, StackPane}
import javafx.scene.text.{Font, Text}
import javafx.stage.{Modality, Stage, WindowEvent}
import javafx.util.StringConverter
import monix.execution.Cancelable
import scala.collection.JavaConverters._
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import suiryc.dl.mngr.model._
import suiryc.scala.misc.Units
import suiryc.dl.mngr.{DownloadManager, I18N, Main, Settings}
import suiryc.dl.mngr.I18N.Strings
import suiryc.dl.mngr.util.Icons
import suiryc.scala.RichOption._
import suiryc.scala.concurrent.Cancellable
import suiryc.scala.javafx.beans.binding.BindingsEx
import suiryc.scala.javafx.beans.property.ConfigEntryProperty
import suiryc.scala.javafx.beans.value.RichObservableValue
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.collections.RichObservableList._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.scene.{Graphics, Styles}
import suiryc.scala.javafx.scene.control.{Dialogs, Panes, TableCellEx, TableViews}
import suiryc.scala.javafx.scene.control.skin.SplitPaneSkinEx
import suiryc.scala.javafx.stage.{StagePersistentView, Stages}
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.settings.ConfigEntry
import suiryc.scala.unused

// TODO: status bar ? (display current speed / limit, number of running downloads + pending + finished / total, number of connections)
class MainController extends StagePersistentView with StrictLogging {

  import MainController._

  @FXML
  protected var menuBar: MenuBar = _

  @FXML
  protected var downloadsMenu: Menu = _

  @FXML
  protected var downloadsStopAllMenu: MenuItem = _

  @FXML
  protected var downloadsResumeAllMenu: MenuItem = _

  @FXML
  protected var downloadsRemoveCompletedMenu: MenuItem = _

  @FXML
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
  protected var dlURIField: TextField = _

  @FXML
  protected var dlServerLink: Hyperlink = _

  @FXML
  protected var dlSiteLink: Hyperlink = _

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
  protected var dlSizeLabel: Label = _

  private val columnDownloadIndex = new TableColumn[UUID, String]("#")

  private val columnDownloadFile = new TableColumn[UUID, UUID](Strings.file)
  private val columnDownloadSize = new TableColumn[UUID, UUID](Strings.size)
  private val columnDownloadDownloaded = new TableColumn[UUID, UUID](Strings.downloaded)
  private val columnDownloadSpeed = new TableColumn[UUID, String](Strings.speed)
  private val columnDownloadEta = new TableColumn[UUID, String](Strings.eta)
  private val columnDownloadSegments = new TableColumn[UUID, String](Strings.segments)

  private val downloadsColumns = List(
    "idx" → columnDownloadIndex,
    "file" → columnDownloadFile,
    "size" → columnDownloadSize,
    "downloaded" → columnDownloadDownloaded,
    "speed" → columnDownloadSpeed,
    "eta" → columnDownloadEta,
    "segments" → columnDownloadSegments
  )

  private val columnLogTime = new TableColumn[LogEntry, String](Strings.time)
  private val columnLogMessage = new TableColumn[LogEntry, LogEntry](Strings.message)

  private val logsColumns = List(
    "time" → columnLogTime,
    "message" → columnLogMessage
  )

  lazy private val stage = splitPane.getScene.getWindow.asInstanceOf[Stage]

  private var actor: ActorRef = _

  @volatile
  private var downloadData: Map[UUID, DownloadData] = Map.empty

  def initialize(state: State): Unit = {
    state.stage.setTitle(Main.versionedName)

    // Note: make the actor name unique (with timestamp) so that it can be
    // recreated later.
    actor = JFXSystem.newJFXActor(
      Props(new ControllerActor(state)),
      s"dl-mngr-main@${System.currentTimeMillis}"
    )

    // We sometimes need to get the current state. So store it as user data.
    state.save()

    // Inject icons in menu
    Icons.setIcons(menuBar)

    // When showing "Downloads" menu, update items state. We could follow table
    // items list and each download state, but that would be a bit overkill.
    // TODO: unfortunately, after a while the menu item remain greyed out even if enabled ...
    //downloadsStopAllMenu.getParentMenu.setOnShowing { _ ⇒
    //  val _data = getDownloadsData
    //  enableMenuStop(downloadsStopAllMenu, _data)
    //  enableMenuResume(downloadsResumeAllMenu, _data)
    //  enableMenuRemoveCompleted(downloadsRemoveCompletedMenu, _data)
    //  enableMenuRemove(downloadsRemove, _data)
    //}

    // We wish to disable mouse events in the custom menu item (but not the
    // nodes within). rateLimitField and rateLimitUnitField are children of an
    // HBox, and its parent will be set upon first displaying the menu.
    // First wait for this to happen.
    rateLimitField.getParent.parentProperty.listen { parent ⇒
      // Now filter all mouse events targeted at this parent.
      parent.addEventFilter(MouseEvent.ANY, (event: MouseEvent) ⇒ {
        if (parent.eq(event.getTarget)) event.consume()
      })
    }
    def getRateLimitValue: Long = {
      try {
        Option(rateLimitField.getText).map(_.toLong).getOrElse(0L)
      } catch {
        case _: Exception ⇒ 0L
      }
    }
    def updateRateLimiter(): Unit = {
      val value = getRateLimitValue
      val unit = rateLimitUnitField.getValue
      val bytesPerSecond = value * unit.factor
      getState.dlMngr.setRateLimit(bytesPerSecond)
      Main.settings.rateLimitValue.set(value)
      Main.settings.rateLimitUnit.set(unit.label)
    }
    rateLimitField.setOnAction { _ ⇒
      updateRateLimiter()
    }
    downloadsMenu.setOnHidden { _ ⇒
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
      case _: Exception ⇒ Units.storage.kibi
    }
    rateLimitUnitField.getSelectionModel.select(unit)
    updateRateLimiter()

    downloadsColumns.foreach(_._2.setSortable(false))
    columnDownloadIndex.setCellFactory { _ ⇒
      new TableCellEx[UUID, String] {
        override protected def itemText(item: String): String = {
          val index = getIndex
          if (index >= 0) (index + 1).toString
          else null
        }
      }
    }
    columnDownloadIndex.setMinWidth(computeTextWidth("_99_"))
    columnDownloadFile.setCellValueFactory { data ⇒
      new SimpleObjectProperty[UUID](data.getValue)
    }
    columnDownloadFile.setCellFactory { _ ⇒
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
            case Some(data) ⇒
              graphicProperty.bind(data.stateIcon)
              BindingsEx.bind(textProperty, data.path) {
                data.path.get.getFileName.toString
              }
              BindingsEx.bind(tooltipProperty, data.path) {
                new Tooltip(data.path.get.toString)
              }
              ()

            case None ⇒
              setGraphic(null)
              setText(null)
              setTooltip(null)
          }
        }
      }
    }
    columnDownloadFile.setMinWidth(computeTextWidth("MMMMMMMMMMMMMMMM") + 24)
    columnDownloadSize.setCellValueFactory { data ⇒
      new SimpleObjectProperty[UUID](data.getValue)
    }
    columnDownloadSize.setCellFactory { _ ⇒
      new TableCell[UUID, UUID] {
        override def updateItem(item: UUID, empty: Boolean): Unit = {
          super.updateItem(item, empty)
          graphicProperty.unbind()
          textProperty.unbind()
          val opt = if (!empty) getDownloadData(item) else None
          opt match {
            case Some(data) ⇒
              BindingsEx.bind(graphicProperty, data.sizeIcon) {
                data.sizeIcon.get
              }
              BindingsEx.bind(textProperty, data.size) {
                data.size.get
              }

            case None ⇒
              setGraphic(null)
              setText(null)
          }
        }
      }
    }
    columnDownloadSize.setMinWidth(computeTextWidth("_9999.9_MiB_"))
    columnDownloadDownloaded.setCellValueFactory { data ⇒
      new SimpleObjectProperty[UUID](data.getValue)
    }
    columnDownloadDownloaded.setCellFactory { _ ⇒
      new TableCellEx[UUID, UUID] {
        getStyleClass.add("table-cell-downloaded")
        override protected def itemText(item: UUID): String = null
        override protected def itemGraphic(item: UUID): Node = {
          getDownloadData(item).map(_.downloadedProgressPane).orNull
        }
      }
    }
    columnDownloadDownloaded.setMinWidth(computeTextWidth("_999.9%_[9999.9_MiB]_"))

    columnDownloadSpeed.setCellValueFactory { data ⇒
      getDownloadData(data.getValue).map(_.rate).getOrElse {
        new SimpleStringProperty()
      }
    }
    columnDownloadSpeed.setMinWidth(computeTextWidth("_9999.9_MiB/s_"))

    columnDownloadEta.setCellValueFactory { data ⇒
      getDownloadData(data.getValue).map(_.eta).getOrElse {
        new SimpleStringProperty()
      }
    }
    columnDownloadEta.setMinWidth(computeTextWidth("_99:99:99_"))

    columnDownloadSegments.setCellValueFactory { data ⇒
      getDownloadData(data.getValue).map(_.segments).getOrElse {
        new SimpleStringProperty()
      }
    }
    columnDownloadSegments.setMinWidth(computeTextWidth("_99/99_"))

    logsColumns.foreach(_._2.setSortable(false))
    val timeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")
    columnLogTime.setCellValueFactory { data ⇒
      Option(data.getValue).map{ v ⇒
        new SimpleStringProperty(v.time.format(timeFormatter))
      }.getOrElse {
        new SimpleStringProperty()
      }
    }
    columnLogTime.setMinWidth(computeTextWidth("_9999-99-99_99:99:99.999_"))

    columnLogMessage.setCellValueFactory { data ⇒
      new SimpleObjectProperty[LogEntry](data.getValue)
    }
    columnLogMessage.setCellFactory { _ ⇒
      new TableCellEx[LogEntry, LogEntry] {
        override protected def itemText(item: LogEntry): String = {
          item.message
        }
        override protected def itemGraphic(item: LogEntry): Node = {
          item.kind match {
            case LogKind.Error ⇒ Icons.exclamationTriangle().pane
            case LogKind.Warning ⇒ Icons.exclamationTriangle(styleClass = List("icon-exclamation-triangle-warning")).pane
            case _ ⇒ Icons.infoCircle().pane
          }
        }
        override protected def updateItem(item: LogEntry, empty: Boolean): Unit = {
          super.updateItem(item, empty)
          val tooltip = Option(item).flatMap(_.exOpt).map { ex ⇒
            val sw = new StringWriter()
            val pw = new PrintWriter(sw)
            ex.printStackTrace(pw)
            new Tooltip(sw.toString)
          }.orNull
          setTooltip(tooltip)
        }
      }
    }

    downloadsTable.setContextMenu(downloadsContextMenu)
    downloadsTable.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
    downloadsTable.getSelectionModel.selectedItemProperty.listen {
      refreshDlLogs()
      refreshDlProperties()
    }

    // Note: 'KEY_PRESSED' and 'KEY_TYPED' are continuously triggered while key
    // remain pressed. 'KEY_RELEASED' is only triggered upon releasing the key.
    // But only 'KEY_TYPED' will give the actual character when combining
    // multiple keys (e.g. 'Shift'+'=' gives '+'). And only 'KEY_PRESSED' or
    // 'KEY_RELEASED' capture keys that do not produce character (like arrows).
    downloadsTable.addEventHandler(KeyEvent.KEY_TYPED, (event: KeyEvent) ⇒ {
      event.getCharacter match {
        case "+" ⇒
          selectedDownloadsData.filter {data ⇒
            data.download.isActive || data.download.canResume
          }.foreach { data ⇒
            getState.dlMngr.addDownloadConnection(data.download.id)
          }

        case "-" ⇒
          selectedDownloadsData.filter(_.download.isRunning).foreach { data ⇒
            getState.dlMngr.removeDownloadConnection(data.download.id)
          }

        case _ ⇒
      }
    })
    downloadsTable.addEventHandler(KeyEvent.KEY_PRESSED, (event: KeyEvent) ⇒ {
      if (event.isControlDown) {
        val most = event.isShiftDown
        event.getCode match {
          case KeyCode.UP ⇒
            moveDownloads(selectedDownloads, up = true, most = most)
            event.consume()

          case KeyCode.DOWN ⇒
            moveDownloads(selectedDownloads, up = false, most = most)
            event.consume()

          case _ ⇒
        }
      } else if (event.getCode == KeyCode.DELETE) {
        // TODO: Shift+Delete = delete file (for failed only ?) downloads ?
        onDownloadsRemove(null)
      }
    })
  }

  private def computeTextWidth(s: String): Double = {
    s.map { c ⇒
      Toolkit.getToolkit.getFontLoader.getFontMetrics(Font.getDefault).getCharWidth(c).toDouble
    }.sum
  }

  private def cancelSubscription(v: Option[Any]): Unit = {
    v match {
      case Some(cancellable: Cancellable) ⇒ cancellable.cancel()
      case _ ⇒
    }
  }

  private def refreshDlLogs(): Unit = {
    // Cancel previous subscriptions if any
    cancelSubscription(Option(logsTable.getUserData))

    // Fix minimum width; will be updated when setting cells content
    columnLogMessage.setMinWidth(computeTextWidth("MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM"))

    selectedDownloadData match {
      case Some(data) ⇒
        val info = data.download.info

        def getLogs(l: java.util.List[_ <: LogEntry]): List[LogEntry] = {
          val logs0 = l.asScala.toList
          if (!Main.settings.debug.get) {
            logs0.filter(_.kind != LogKind.Debug)
          } else {
            logs0
          }
        }
        def setLogs(): Unit = {
          val logs = getLogs(info.logs)
          logsTable.getItems.setAll(logs:_*)
          updateLogMessageMinWidth(logs)
          ()
        }

        def updateLogMessageMinWidth(els: List[LogEntry]): Unit = {
          if (els.nonEmpty) {
            val textWidth = els.map(entry ⇒ computeTextWidth(entry.message)).max
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

        // Listen to this download logs.
        // We only expect entries to be added, which can be propagated easily in
        // the table. For any other change, re-set all the items.
        // In either case, update the column minimum width accordingly.
        val logsCancellable = info.logs.listen { change ⇒
          def loop(): Unit = {
            if (change.next()) {
              if (change.wasPermutated() || change.wasUpdated() || change.wasRemoved() || !change.wasAdded()) {
                setLogs()
              } else {
                val logs = getLogs(change.getAddedSubList)
                if (logs.nonEmpty) {
                  // We only expect adding at the end. Adding at position
                  // 'change.getFrom' fails when filtering debug messages.
                  logsTable.getItems.addAll(logs.asJava)
                  updateLogMessageMinWidth(logs)
                }
                loop()
              }
            }
          }

          JFXSystem.runLater {
            loop()
          }
        }
        // Set initial value (only changes are listened)
        setLogs()

        // Remember this subscription
        logsTable.setUserData(logsCancellable)

      case None ⇒
        logsTable.getItems.clear()
    }
  }

  private def refreshDlProperties(): Unit = {
    cancelSubscription(Option(dlPropertiesTab.getUserData))

    selectedDownloadData match {
      case Some(data) ⇒
        val download = data.download
        val info = download.info

        List(dlServerLink, dlSiteLink).foreach { field ⇒
          field.setDisable(false)
        }
        dlSiteLink.setText(download.siteSettings.site)
        dlReferrerField.setText(download.referrer.map(_.toString).orNull)
        dlCookieField.setText(download.cookie.orNull)
        dlUserAgentField.setText(download.userAgent.orNull)
        def updateProperties(): Unit = {
          dlURIField.setText(info.uri.get.toString)
          dlServerLink.setText(info.uri.get.getHost)
          dlFolderField.setText(info.path.get.getParent.toString)
          dlFileField.setText(info.path.get.getFileName.toString)
          dlSizeLabel.setText(if (info.isSizeDetermined && !info.isSizeUnknown) info.size.get.toString else "")
          // Somehow changed text (e.g. dlSiteLink) is not always updated until
          // layout is triggered again (e.g. resizing). Try manual trigger.
          dlPropertiesScrollPane.layout()
        }
        val propertiesCancellable = RichObservableValue.listen(info.uri, info.path, info.size) {
          JFXSystem.runLater {
            updateProperties()
          }
        }
        updateProperties()
        // Remember this subscription
        dlPropertiesTab.setUserData(propertiesCancellable)

      case None ⇒
        List(dlServerLink, dlSiteLink).foreach { field ⇒
          field.setDisable(true)
        }
        List(dlServerLink, dlSiteLink, dlSizeLabel).foreach { field ⇒
          field.setText(null)
        }
        List(dlURIField, dlReferrerField, dlCookieField, dlUserAgentField,
          dlFolderField, dlFileField).foreach { field ⇒
          field.setText(null)
        }
    }
  }

  /** Restores (persisted) view. */
  override protected def restoreView(): Unit = {
    Stages.onStageReady(stage, first = true) {
      // Restore stage location
      Stages.setMinimumDimensions(stage)
      stageLocation.opt.foreach { loc ⇒
        Stages.setLocation(stage, loc, setSize = true)
      }

      val state = getState
      state.dlMngr.start()
      state.dlMngr.getDownloads.foreach { download ⇒
        addDownload(download.id, first = false, select = false)
      }
    }(JFXSystem.dispatcher)

    // Restore SplitPane divider positions
    splitPaneDividerPositions.opt.foreach { dividerPositions ⇒
      Panes.restoreDividerPositions(splitPane, dividerPositions)
    }

    // Restore columns order and width
    TableViews.setColumnsView(downloadsTable, downloadsColumns, downloadsColumnsPref.opt)
    TableViews.setColumnsView(logsTable, logsColumns, logsColumnsPref.opt)

    // Now what we want is for both columns to occupy the whole table width.
    // Using a constrained resizing policy gets in the way of restoring the
    // view, so a solution is to create a binding through which we set the
    // 'message' column (preferred) width according to the width of other
    // elements:
    //  column2Width = tableWidth - tablePadding - column1Width
    // However the vertical scrollbar which may appear is not taken into
    // account in table width. It is in the "clipped-container" that is a
    // Region of the viewed content:
    //  column2Width = containerWidth - column1Width
    // (the container has 0 width when there is no content in the table)
    //
    // The table width is changed before the container one, which triggers
    // glitches when resizing down using the second formula: the horizontal
    // scrollbar appears (and disappears upon interaction or resizing up).
    // Requesting layout (in 'runLater') makes it disappear right away.
    // But listening to table width too (which is changed first) and keeping
    // the minimum width between 'tableWidth - tablePadding - scrollBarWidth'
    // and 'containerWidth' prevents the horizontal scrollbar from appearing.
    // TODO: check if all this is still right ? (especially when resizing down)
    //
    // Extra note: column width may be a decimal value. Keep floor value of
    // target column width to also prevent horizontal scrollbar from appearing.
    // Extra note: since we change the minimum width, listen to it to enforce
    // value when necessary.
    val clippedContainer = logsTable.lookup(".clipped-container").asInstanceOf[Region]
    val scrollBar = logsTable.lookupAll(".scroll-bar").asScala.collect {
      case scrollBar: VirtualScrollBar if scrollBar.getPseudoClassStates.asScala.map(_.getPseudoClassName).contains("vertical") ⇒ scrollBar
    }.head

    def updateColumnWidth(): Unit = {
      val insets = logsTable.getPadding
      val padding = insets.getLeft + insets.getRight
      val scrollbarWidth =
        if (!scrollBar.isVisible) 0
        else scrollBar.getWidth
      val logsWidth0 = logsTable.getWidth - padding - scrollbarWidth
      val logsWidth =
        if (clippedContainer.getWidth > 0) math.min(logsWidth0, clippedContainer.getWidth)
        else logsWidth0
      val width = (logsWidth - columnLogTime.getWidth).floor
      columnLogMessage.setPrefWidth(width)
      // Setting max width helps applying target width in some cases (minimum
      // width changed to a lower value after re-setting logs).
      val maxWidth = math.max(width, columnLogMessage.getMinWidth)
      columnLogMessage.setMaxWidth(maxWidth)
    }

    RichObservableValue.listen[AnyRef](logsTable.widthProperty, clippedContainer.widthProperty,
      columnLogTime.widthProperty, columnLogMessage.minWidthProperty)
    {
      updateColumnWidth()
    }
    ()
  }

  /** Persists view (stage location, ...). */
  override protected def persistView(): Unit = {
    // Persist stage location
    // Note: if iconified, resets it
    stageLocation.set(Stages.getLocation(stage).orNull)

    // Persist table columns order and width
    downloadsColumnsPref.set(TableViews.getColumnsView(downloadsTable, downloadsColumns))
    logsColumnsPref.set(TableViews.getColumnsView(logsTable, logsColumns))

    // Persist SplitPane divider positions
    splitPaneDividerPositions.set(Panes.encodeDividerPositions(splitPane))
  }

  private def getState: State = {
    stage.getUserData.asInstanceOf[State]
  }

  private def selectedDownload: Option[UUID] = Some(downloadsTable.getSelectionModel.getSelectedItem)
  private def selectedDownloadData: Option[DownloadData] = selectedDownload.flatMap(getDownloadData)

  private def selectedDownloadsIdx: List[Int] = downloadsTable.getSelectionModel.getSelectedIndices.asScala.toList.map(Int.unbox)
  private def selectedDownloads: List[UUID] = downloadsTable.getSelectionModel.getSelectedItems.asScala.toList
  private def selectedDownloadsData: List[DownloadData] = selectedDownloads.flatMap(getDownloadData)

  private def getDownloadsData: List[DownloadData] = downloadsTable.getItems.asScala.toList.flatMap(getDownloadData)

  private def getDownloadData(id: UUID): Option[DownloadData] = this.synchronized {
    downloadData.get(id)
  }

  private def setDownloadData(id: UUID, data: DownloadData): Unit = this.synchronized {
    downloadData += (id → data)
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
    menu.setDisable(!data.exists(_.download.isDone))
  }

  private def enableMenuRemove(menu: MenuItem, data: List[DownloadData]): Unit = {
    menu.setDisable(data.forall(_.download.isActive))
  }

  private val downloadsContextMenu: ContextMenu = {
    val contextMenu = new ContextMenu()

    val stopDownload = new MenuItem(Strings.stop)
    stopDownload.setGraphic(Icons.stop().pane)
    stopDownload.setOnAction { _ ⇒
      selectedDownloadsData.filter(_.download.canStop).foreach { data ⇒
        getState.dlMngr.stopDownload(data.download.id)
      }
    }
    val resumeDownload = new MenuItem(Strings.resume)
    resumeDownload.setGraphic(Icons.play().pane)
    resumeDownload.setOnAction { _ ⇒
      selectedDownloadsData.filter(_.download.canResume(restart = false)).foreach { data ⇒
        getState.dlMngr.resumeDownload(data.download.id, reusedOpt = None, restart = false)
      }
    }
    val restartDownload = new MenuItem(Strings.restart)
    restartDownload.setGraphic(Icons.undo().pane)
    restartDownload.setOnAction { _ ⇒
      selectedDownloadsData.filter(_.download.canResume(restart = true)).foreach { data ⇒
        getState.dlMngr.resumeDownload(data.download.id, reusedOpt = None, restart = true)
      }
    }

    val stopAll = new MenuItem(Strings.stopAll)
    stopAll.setGraphic(Icons.stop().pane)
    stopAll.setOnAction(onDownloadsStopAll)
    val resumeAll = new MenuItem(Strings.resumeAll)
    resumeAll.setGraphic(Icons.play().pane)
    resumeAll.setOnAction(onDownloadsResumeAll)
    val removeCompleted = new MenuItem(Strings.removeCompleted)
    removeCompleted.setGraphic(Icons.eraser().pane)
    removeCompleted.setOnAction(onDownloadsRemoveCompleted)
    val remove = new MenuItem(Strings.remove)
    remove.setGraphic(Icons.minus().pane)
    remove.setOnAction(onDownloadsRemove)

    val moveFirst = new MenuItem(Strings.moveFirst)
    moveFirst.setGraphic(Icons.angleDoubleUp().pane)
    moveFirst.setOnAction { _ ⇒
      moveDownloads(selectedDownloads, up = true, most = true)
    }
    val moveUp = new MenuItem(Strings.moveUp)
    moveUp.setGraphic(Icons.angleUp().pane)
    moveUp.setOnAction { _ ⇒
      moveDownloads(selectedDownloads, up = true, most = false)
    }
    val moveDown = new MenuItem(Strings.moveDown)
    moveDown.setGraphic(Icons.angleDown().pane)
    moveDown.setOnAction { _ ⇒
      moveDownloads(selectedDownloads, up = false, most = false)
    }
    val moveLast = new MenuItem(Strings.moveLast)
    moveLast.setGraphic(Icons.angleDoubleDown().pane)
    moveLast.setOnAction { _ ⇒
      moveDownloads(selectedDownloads, up = false, most = true)
    }

    contextMenu.getItems.addAll(
      stopDownload, resumeDownload, restartDownload,
      new SeparatorMenuItem,
      stopAll, resumeAll, removeCompleted, remove,
      new SeparatorMenuItem,
      moveFirst, moveUp, moveDown, moveLast
    )
    contextMenu.setOnShowing { _ ⇒
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
      title.map(v ⇒ s" title=<$v>").getOrElse("")
    }${
      contentText.map(v ⇒ s" text=<$v>").getOrElse("")
    } ${ex.getMessage}"
    logger.error(msg, ex)
    Dialogs.error(
      owner = Some(stage),
      title = title.orElse(Some(stage.getTitle)),
      contentText = contentText.orElse(Some(Strings.unexpectedIssue)),
      ex = Some(ex)
    )
    ()
  }

  def addDownload(dlInfo: NewDownloadInfo): Future[Unit] = {
    val promise = Promise[Unit]()
    actor ! OnDownloadsAdd(dlInfo, promise)
    promise.future
  }

  def onCloseRequest(event: WindowEvent): Unit = {
    actor ! OnExit
    // Note: consume the event, the actor is responsible for shutting down
    event.consume()
  }

  def onOptions(@unused event: ActionEvent): Unit = {
    actor ! OnOptions()
  }

  def onExit(@unused event: ActionEvent): Unit = {
    actor ! OnExit
  }

  def onDownloadsAdd(@unused event: ActionEvent): Unit = {
    addDownload(NewDownloadInfo())
    ()
  }

  def onDownloadsStopAll(@unused event: ActionEvent): Unit = {
    getDownloadsData.filter(_.download.canStop).foreach { data ⇒
      getState.dlMngr.stopDownload(data.download.id)
    }
  }

  def onDownloadsResumeAll(@unused event: ActionEvent): Unit = {
    getDownloadsData.filter(_.download.canResume(restart = false)).foreach { data ⇒
      getState.dlMngr.resumeDownload(data.download.id, reusedOpt = None, restart = false)
    }
  }

  def onDownloadsRemoveCompleted(@unused event: ActionEvent): Unit = {
    actor ! OnDownloadsRemoveCompleted
  }

  def onDownloadsRemove(@unused event: ActionEvent): Unit = {
    actor ! OnDownloadsRemove
  }

  def onDlServerSettings(@unused event: ActionEvent): Unit = {
    selectedDownloadData.foreach { data ⇒
      val host = data.download.info.uri.get.getHost
      actor ! OnOptions(OptionsController.Display(serverSettings = Some(host)))
    }
  }

  def onDlSiteSettings(@unused event: ActionEvent): Unit = {
    selectedDownloadData.foreach { data ⇒
      val siteSettings = data.download.siteSettings
      actor ! OnOptions(OptionsController.Display(siteSettings = Some(siteSettings)))
    }
  }

  def addDownload(id: UUID, first: Boolean, select: Boolean): Unit = {
    actor ! AddDownload(id, first, select)
  }

  def moveDownloads(ids: List[UUID], up: Boolean, most: Boolean): Unit = {
    actor ! MoveDownloads(ids, up, most)
  }

  class ControllerActor(state0: State) extends Actor {

    override def receive: Receive = receive(state0)

    // Wrap actual partial function in a try/catch to display unexpected issues.
    // Otherwise window becomes unusable (akka messages goes to dead letters).
    def receive(state: State): Receive =
      new PartialFunction[Any, Unit]() {
        val r: Receive = receive0(state)
        override def isDefinedAt(x: Any): Boolean = r.isDefinedAt(x)
        override def apply(x: Any): Unit = try {
          r.apply(x)
        } catch {
          case ex: Exception ⇒
            displayError(
              title = Some(state.stage.getTitle),
              contentText = Some(Strings.unexpectedIssue),
              ex = ex
            )
            ()
        }
      }

    def receive0(state: State): Receive = {
      case OnOptions(display)              ⇒ onOptions(state, display)
      case OnExit                          ⇒ onExit(state)
      case OnDownloadsAdd(dlInfo, promise) ⇒ onDownloadsAdd(state, dlInfo, promise)
      case OnDownloadsRemoveCompleted      ⇒ onDownloadsRemoveCompleted(state)
      case OnDownloadsRemove               ⇒ onDownloadsRemove(state)
      case AddDownload(id, first, select)  ⇒ addDownload(state, id, first, select)
      case MoveDownloads(ids, up, most)    ⇒ moveDownloads(state, ids, up, most)
    }

    def onExit(state: State): Unit = {
      // TODO: check there is no running download, or ask for confirmation to stop them
      persistView()

      state.dlMngr.stop().onComplete { _ ⇒
        val ok = try {
          state.dlMngr.saveState()
          true
        } catch {
          case ex: Exception ⇒
            displayError(
              title = Some(state.stage.getTitle),
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

    def onOptions(state: State, display: OptionsController.Display): Unit = {
      val dlMngr = state.dlMngr
      val debug0 = Main.settings.debug.get
      val dialog = OptionsController.buildDialog(state.stage, display)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.setResizable(true)
      val result = dialog.showAndWait().orElse(OptionsController.Result())

      // Refresh selected DL logs to show/hide debug events.
      if (Main.settings.debug.get != debug0) {
        refreshDlLogs()
      }

      // If sites or limits were changed, refresh downloads: in particular the
      // maximum number of segments will be recomputed, and the current
      // connections associated to the correct site.
      if (result.sitesChanged || result.cnxLimitChanged) {
        dlMngr.refreshDownloads()
      }

      // If sites were changed, refresh properties (selected download site may
      // have changed).
      if (result.sitesChanged) {
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

    def onDownloadsAdd(state: State, dlInfo: NewDownloadInfo, promise: Promise[Unit]): Unit = {
      val dialogOpt = NewDownloadController.buildDialog(MainController.this, state.stage, state.dlMngr, dlInfo)
      // In automatic mode, there is no dialog and download has been added.
      dialogOpt.foreach { dialog ⇒
        dialog.initModality(Modality.WINDOW_MODAL)
        dialog.setResizable(true)
        dialog.show()
      }
      promise.trySuccess(())
      ()
    }

    def onDownloadsRemoveCompleted(state: State): Unit = {
      getDownloadsData.foreach { data ⇒
        if (data.download.isDone) {
          removeDownload(state, data.download.id)
        }
      }
    }

    def onDownloadsRemove(state: State): Unit = {
      // We don't remove 'active' downloads
      val selected = selectedDownloadsData.filter(!_.download.isActive)

      // We can safely remove if done (success) or not started (failure or not)
      val safe = selected.filter { data ⇒
        data.download.isDone || !data.download.isStarted
      }
      safe.foreach { data ⇒
        removeDownload(state, data.download.id)
      }

      // Get remaining selected downloads (started and unfinished)
      val unfinished = selected.filterNot(safe.contains)
      if (unfinished.nonEmpty) {
        // Build simple dialog to confirm removal
        val buttonRemove = new ButtonType(Strings.remove)
        val dialog = new Dialog[Option[Boolean]]()
        Stages.initOwner(dialog, stage)
        dialog.getDialogPane.getButtonTypes.addAll(buttonRemove, ButtonType.CANCEL)
        Dialogs.setDefaultButton(dialog, buttonRemove)
        val loader = new FXMLLoader(getClass.getResource("/fxml/remove-unfinished.fxml"), I18N.getResources)
        dialog.getDialogPane.setContent(loader.load[Node]())

        // Insert list of downloads in message field
        val messageField = dialog.getDialogPane.lookup("#messageField").asInstanceOf[Label]
        messageField.setText(s"${messageField.getText}\n${unfinished.map(_.path.get.getFileName).mkString("\n")}")

        // Track (and persist) whether to remove from disk too
        val removeFromDiskField = dialog.getDialogPane.lookup("#removeFormDiskField").asInstanceOf[CheckBox]
        removeFromDiskField.setSelected(removeUnfinishedFromDisk.get)
        BindingsEx.bind(removeUnfinishedFromDisk, removeFromDiskField.selectedProperty) {
          removeFromDiskField.isSelected
        }

        // Determine the dialog result
        dialog.setResultConverter {
          case `buttonRemove` ⇒ Some(removeFromDiskField.isSelected)
          case _ ⇒ None
        }

        // Show, wait and apply action
        dialog.showAndWait().flatten.foreach { removeFromDisk ⇒
          unfinished.foreach { data ⇒
            val download = data.download
            // Remove download entry
            removeDownload(state, download.id)
            // Remove from disk when requested
            if (removeFromDisk) {
              // Since the download is unfinished, we only need to delete the
              // temporary file if any (or the real file otherwise).
              download.downloadFile.getTemporaryPath.getOrElse(download.downloadFile.getPath).toFile.delete()
            }
          }
        }
      }
    }

    def removeDownload(state: State, id: UUID): Unit = {
      // Remove this download from the manager, our own data, and the table items
      state.dlMngr.removeDownload(id)
      removeDownloadData(id)
      downloadsTable.getItems.remove(id)
      ()
    }

    def addDownload(state: State, id: UUID, first: Boolean, select: Boolean): Unit = {
      import scala.collection.JavaConverters._
      val items = downloadsTable.getItems
      if (!items.asScala.toSet.contains(id)) {
        state.dlMngr.getDownload(id).foreach { download ⇒
          val info = download.info
          val data = new DownloadData(download)
          setDownloadData(id, data)

          BindingsEx.jfxBind(data.path, info.path) {
            info.path.get
          }

          def displaySize: String = {
            val size = if (info.isSizeDetermined) Option(info.size.get) else download.sizeHint
            size.filter(_ >= 0).map { size ⇒
              Units.storage.toHumanReadable(size)
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
                (if (!download.acceptRanges.contains(false)) Nil else List(Strings.resumeUnsupported))

            if (warnings.nonEmpty) {
              val icon = Icons.exclamationTriangle(styleClass = List("icon-exclamation-triangle-warning")).pane
              val tooltip = new Tooltip(warnings.mkString("\n"))
              Tooltip.install(icon, tooltip)
              icon
            } else null
          }.bind(info.size, info.acceptRanges)

          BindingsEx.bind(data.downloadedProgress.progressProperty, 500.millis, JFXSystem.scheduler, info.downloaded, info.state) {
            Option(info.size.get).filter(_ >= 0).map { size ⇒
              val v =
                if (size > 0) info.downloaded.getValue.toDouble / size
                else 0.0
              v
            }.getOrElse {
              // Note: Usually we would set progress to -1.0 if isSizeDetermined
              // when running. But the "indeterminate state" animation consumes
              // more CPU (and apparently too much under Linux).
              info.state.get match {
                case DownloadState.Running ⇒ 0.0
                case DownloadState.Success ⇒ 1.0
                case _ ⇒ 0.0
              }
            }:Double
          }
          BindingsEx.bind(data.downloadedProgressText.textProperty, 500.millis, JFXSystem.scheduler, info.downloaded) {
            val downloaded = info.downloaded.get
            val size = info.size.get
            if (downloaded <= 0) null
            else if (size > 0) {
              val percent = downloaded * 100.0 / size
              s"%.1f%% [%s]".format(percent, Units.storage.toHumanReadable(downloaded))
            } else {
              Units.storage.toHumanReadable(downloaded)
            }
          }

          // Note:
          // It is sensible to use the same duration for the rate display
          // throttling and the rate handler step.
          new BindingsEx.Builder(JFXSystem.scheduler).add(data.rate) {
            if (download.state == DownloadState.Running) {
              val rate = data.rateHandler.update(info.downloaded.get)
              s"%s/s".format(Units.storage.toHumanReadable(rate))
            } else {
              null
            }
          }.add(data.eta) {
            if (download.state == DownloadState.Running) {
              Option(info.size.get).filter(_ > 0).map { size ⇒
                val remaining = size - info.downloaded.getValue
                val rate = data.rateHandler.update(info.downloaded.get)
                if (rate > 0) {
                  val seconds = (remaining.toDouble / rate).ceil.toLong
                  s"%02d:%02d:%02d".format(seconds / (60 * 60), (seconds / 60) % 60, seconds % 60)
                } else "∞"
              }.orNull
            } else {
              null
            }
          }.bind(data.rateHandler.step, info.downloaded, info.state, data.rateUpdate)

          new BindingsEx.Builder(JFXSystem.scheduler).add(data.stateIcon) {
            download.state match {
              case DownloadState.Pending ⇒
                Icons.hourglass().pane

              case DownloadState.Running ⇒
                val styleClass = if (download.activeSegments == 0) List("icon-play-started") else Nil
                Icons.play(styleClass = styleClass).pane

              case DownloadState.Success ⇒
                if (Main.settings.removeCompleted.get) removeDownload(state, data.download.id)
                Icons.checkSquare().pane

              case DownloadState.Stopped ⇒
                Icons.stop().pane

              case DownloadState.Failure ⇒
                Icons.exclamationTriangle().pane
            }
          }.add(data.segments) {
            if (download.state == DownloadState.Running) {
              s"${info.activeSegments.get}/${info.maxSegments.get}"
            } else {
              null
            }
          }.bind(info.state, info.activeSegments, info.maxSegments)

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
    def moveDownloads(state: State, ids: List[UUID], up: Boolean, most: Boolean): Unit = {
      // Save current selection
      val selectedItem = downloadsTable.getSelectionModel.getSelectedItem
      val selectedItems = selectedDownloads.toSet
      // Clear selection before moving item
      downloadsTable.getSelectionModel.clearSelection()

      val items = downloadsTable.getItems
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

      if (up) {
        // When moving up, use the first item as base.
        val dst = if (most) 0 else math.max(0, items.indexOf(ids.head) - 1)
        ids.zipWithIndex.foreach {
          case (id, offset) ⇒ moveDownload(id, dst + offset)
        }
      } else {
        // When moving down, use the last item as base.
        val idxMax = items.size - 1
        val dst = if (most) idxMax else math.min(idxMax, items.indexOf(ids.last) + 1)
        // Since we are moving each item downward - from first to last -, they
        // individually all go to the same position so that at the end the last
        // one is really moved to the target position while others are right
        // before it (and remain in original order).
        ids.foreach { id ⇒
          moveDownload(id, dst)
        }
      }

      // Re-order downloads in manager
      state.dlMngr.reorderDownloads(items.asScala.toList)

      // Restore selected items: the last one to be selected will be the one
      // returned by one-item getters.
      (selectedItems - selectedItem).foreach(downloadsTable.getSelectionModel.select)
      downloadsTable.getSelectionModel.select(selectedItem)
      ()
    }

  }

}

object MainController {

  case class State(
    stage: Stage,
    dlMngr: DownloadManager
  ) {
    def save(): Unit = stage.setUserData(this)
  }

  class DownloadData(var download: Download) {
    // Notes:
    // In the downloads table, we may wish to bind some Cell properties to a
    // download. Since the cell item may change, we must be able to 'reset' the
    // binding easily.
    // As most - if not all - download properties are not updated within JavaFX
    // we need to go through BindindsEx (no real bindings, but Cancellables).
    // The easiest way is then to create an intermediate Property here, which is
    // updated though BindingsEx (created when download is added) and use a
    // normal Binding in the table cells (with which we can simply 'unbind').
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
    val rateUpdate: SimpleBooleanProperty = new SimpleBooleanProperty()
    private var rateUpdateCancellable: Option[Cancelable] = None

    def refreshRateUpdate(state: DownloadState.Value): Unit = this.synchronized {
      val running = state == DownloadState.Running
      rateUpdateCancellable match {
        case Some(cancellable) ⇒
          if (!running) {
            cancellable.cancel()
            rateUpdateCancellable = None
          }

        case None ⇒
          if (running) {
            rateUpdateCancellable = Some(Main.scheduler.scheduleWithFixedDelay(1.second, 1.second) {
              // Inverting the current value is a little trick that validates
              // the observable ('get') and immediately invalidates it ('set'
              // with different value). With this, any listener is guaranteed
              // to be notified right now.
              rateUpdate.set(!rateUpdate.get)
            })
          }
      }
    }

    download.info.state.listen(refreshRateUpdate _)
    refreshRateUpdate(download.state)

    val rateHandler = new RateHandler(download.info.downloaded.get, 4.seconds, 800.millis)

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

  case class OnOptions(display: OptionsController.Display = OptionsController.Display())
  case object OnExit
  case class OnDownloadsAdd(dlInfo: NewDownloadInfo, promise: Promise[Unit])
  case object OnDownloadsRemoveCompleted
  case object OnDownloadsRemove
  case class AddDownload(id: UUID, first: Boolean, select: Boolean)
  case class MoveDownloads(ids: List[UUID], up: Boolean, most: Boolean)

  private val settingsKeyPrefix = "main"

  private val stageLocation = ConfigEntry.from[StageLocation](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_DL_MNGR, Settings.KEY_STAGE, settingsKeyPrefix, "location")

  private val splitPaneDividerPositions: ConfigEntry[String] = ConfigEntry.from[String](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_DL_MNGR, Settings.KEY_STAGE, settingsKeyPrefix, "splitPane", "dividerPositions")

  private val downloadsColumnsPref: ConfigEntry[String] = ConfigEntry.from[String](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_DL_MNGR, Settings.KEY_STAGE, settingsKeyPrefix, "downloads", "columns")

  private val logsColumnsPref: ConfigEntry[String] = ConfigEntry.from[String](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_DL_MNGR, Settings.KEY_STAGE, settingsKeyPrefix, "logs", "columns")

  private val removeUnfinishedFromDisk = ConfigEntryProperty {
    ConfigEntry.from[Boolean](Main.settings.settings,
      Settings.KEY_SUIRYC, Settings.KEY_DL_MNGR, Settings.KEY_STAGE, "remove-unfinished", "remove-from-disk")
  }

  def build(state: State): Unit = {
    val stage = state.stage
    val first = Option(stage.getScene).isEmpty
    // Set stage icons.
    // The bare minimum is 32x32 (fits well when resized to 16x16).
    List(256.0, 128.0, 64.0, 32.0, 16.0).foreach { size ⇒
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
