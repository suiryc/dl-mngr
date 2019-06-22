package suiryc.dl.mngr.controllers

import javafx.fxml.FXMLLoader
import javafx.scene.Node
import javafx.scene.control.{Alert, ButtonType, CheckBox, Dialog, Label}
import javafx.stage.Window
import suiryc.dl.mngr.{I18N, Main, Settings}
import suiryc.scala.javafx.beans.binding.BindingsEx
import suiryc.scala.javafx.beans.property.ConfigEntryProperty
import suiryc.scala.javafx.scene.control.Dialogs
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.settings.ConfigEntry

object RemoveUnfinishedController {

  import I18N.Strings

  private val removeUnfinishedFromDisk = ConfigEntryProperty {
    ConfigEntry.from[Boolean](Main.settings.settings,
      Settings.prefix ++ Seq(Settings.KEY_STAGE, "remove-unfinished", "remove-from-disk"))
  }

  def buildDialog(owner: Window, unfinished: List[MainController.DownloadData]): Dialog[Option[Boolean]] = {
    // Build simple dialog to confirm removal
    val buttonRemove = new ButtonType(Strings.remove)
    val dialog = new Dialog[Option[Boolean]]()
    Stages.initOwner(dialog, owner)
    dialog.setTitle(Dialogs.getAlertTitle(Alert.AlertType.CONFIRMATION))
    dialog.getDialogPane.getButtonTypes.addAll(buttonRemove, ButtonType.CANCEL)
    Dialogs.setDefaultButton(dialog, buttonRemove)
    val loader = new FXMLLoader(getClass.getResource("/fxml/remove-unfinished.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load[Node]())

    // Insert list of downloads in message field
    val messageField = dialog.getDialogPane.lookup("#messageField").asInstanceOf[Label]
    messageField.setText(s"${messageField.getText}\n${unfinished.map(_.download.downloadFile.getWorkingPath.getFileName).mkString("\n")}")

    // Track (and persist) whether to remove from disk too
    val removeFromDiskField = dialog.getDialogPane.lookup("#removeFromDiskField").asInstanceOf[CheckBox]
    removeFromDiskField.setSelected(removeUnfinishedFromDisk.get)
    BindingsEx.bind(removeUnfinishedFromDisk, removeFromDiskField.selectedProperty) {
      removeFromDiskField.isSelected
    }

    // Determine the dialog result
    dialog.setResultConverter {
      case `buttonRemove` ⇒ Some(removeFromDiskField.isSelected)
      case _ ⇒ None
    }

    dialog
  }

}
