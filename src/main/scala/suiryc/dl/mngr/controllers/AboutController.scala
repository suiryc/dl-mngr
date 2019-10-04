package suiryc.dl.mngr.controllers

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}
import javafx.beans.property.SimpleStringProperty
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Node
import javafx.scene.control.{ButtonType, Dialog, Label, TableColumn, TableView}
import javafx.scene.text.Font
import javafx.stage.Window
import suiryc.dl.mngr.util.Icons
import suiryc.dl.mngr.{I18N, Info}
import suiryc.scala.javafx.scene.Graphics
import suiryc.scala.javafx.scene.text.Fonts
import suiryc.scala.javafx.stage.Stages

class AboutController {

  import AboutController._

  @FXML
  protected var versionLabel: Label = _

  @FXML
  protected var scalaVersionLabel: Label = _

  @FXML
  protected var sbtVersionLabel: Label = _

  @FXML
  protected var gitCommitLabel: Label = _

  @FXML
  protected var buildTimeLabel: Label = _

  @FXML
  protected var dependenciesTable: TableView[Dependency] = _

  // Reminder: non-private 'initialize' (without parameters) is automatically
  // called as part of FXML building, so make the function private since we
  // call it when appropriate already.
  private def initialize(): Unit = {
    import scala.Ordering.Double.TotalOrdering

    versionLabel.setText(Info.version)
    scalaVersionLabel.setText(Info.scalaVersion)
    sbtVersionLabel.setText(Info.sbtVersion)
    gitCommitLabel.setText(Info.gitHeadCommit.orNull)
    val buildTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(Info.buildTime), ZoneId.systemDefault)
    buildTimeLabel.setText(buildTime.format(timeFormatter))

    val dependencies = Info.libraryDependencies.flatMap { dep =>
      val split = dep.split(':').reverse
      val version = split.head
      // Ignore dependencies used in 'test' setup.
      if (version == "test") None
      else {
        val name = split.tail.reverse.mkString(":")
        Some(Dependency(name, version))
      }
    }.sortBy(_.name)
    val dependencyNameColumn = dependenciesTable.getColumns.get(0).asInstanceOf[TableColumn[Dependency, String]]
    dependencyNameColumn.setCellValueFactory { data =>
      new SimpleStringProperty(Option(data.getValue).map(_.name).orNull)
    }
    dependencyNameColumn.setMinWidth(dependencies.map(_.name).map(computeTextWidth).max + 10)
    val dependencyVersionColumn = dependenciesTable.getColumns.get(1).asInstanceOf[TableColumn[Dependency, String]]
    dependencyVersionColumn.setCellValueFactory { data =>
      new SimpleStringProperty(Option(data.getValue).map(_.version).orNull)
    }
    dependencyVersionColumn.setMinWidth(dependencies.map(_.version).map(computeTextWidth).max + 10)
    dependenciesTable.getItems.addAll(dependencies.toSeq:_*)
    ()
  }

}

object AboutController {

  import I18N.Strings

  case class Dependency(name: String, version: String)

  private val timeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")

  private def computeTextWidth(s: String): Double = Fonts.textWidth(Font.getDefault, s)

  def buildDialog(owner: Window): Dialog[Unit] = {
    val dialog = new Dialog[Unit]()
    Stages.initOwner(dialog, owner)
    Stages.getStage(dialog).getIcons.clear()
    List(256.0, 128.0, 64.0, 32.0, 16.0).foreach { size =>
      val icon = Icons.infoCircle(targetSvgSize = size)
      // We want to apply CSS, but for it to work properly there must be a
      // "root" element (which holds some JavaFX CSS variables).
      icon.pane.getStyleClass.add("root")
      icon.pane.getStylesheets.add(getClass.getResource("/css/main.css").toExternalForm)
      Stages.getStage(dialog).getIcons.add(Graphics.buildImage(icon.pane))
    }
    dialog.setTitle(Strings.about)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.CLOSE)

    val loader = new FXMLLoader(getClass.getResource("/fxml/about.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load[Node]())
    val controller = loader.getController[AboutController]
    controller.initialize()

    dialog
  }

}
