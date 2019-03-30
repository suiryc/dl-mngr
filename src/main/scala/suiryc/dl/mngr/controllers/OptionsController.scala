package suiryc.dl.mngr.controllers

import java.nio.file.Paths
import javafx.collections.FXCollections
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Node
import javafx.scene.control._
import javafx.stage.{DirectoryChooser, Stage, Window}
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import suiryc.dl.mngr.util.Icons
import suiryc.dl.mngr.{I18N, Main, Settings}
import suiryc.scala.javafx.beans.binding.BindingsEx
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.scene.{Graphics, Styles}
import suiryc.scala.javafx.scene.control.{CellWithSeparator, Dialogs, I18NLocaleCell}
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.javafx.stage.{PathChoosers, StageLocationPersistentView, Stages}
import suiryc.scala.misc.{Units, Util}
import suiryc.scala.settings._
import suiryc.scala.unused
import suiryc.scala.util.I18NLocale

class OptionsController extends StageLocationPersistentView(OptionsController.stageLocation) {

  import I18N.Strings
  import OptionsController._

  @FXML
  protected var languageChoice: ComboBox[I18NLocale] = _

  @FXML
  protected var debugField: CheckBox = _

  @FXML
  protected var downloadFolderField: TextField = _

  @FXML
  protected var downloadFolderSelectButton: Button = _

  @FXML
  protected var fileExtensionField: TextField = _

  @FXML
  protected var removeCompletedField: CheckBox = _

  @FXML
  protected var maxDownloadsField: TextField = _

  @FXML
  protected var maxCnxField: TextField = _

  @FXML
  protected var maxServerCnxField: TextField = _

  @FXML
  protected var maxSegmentsField: TextField = _

  @FXML
  protected var minSegmentSizeField: TextField = _

  @FXML
  protected var preallocateField: CheckBox = _

  @FXML
  protected var preallocateZeroField: CheckBox = _

  @FXML
  protected var writeBufferSizeField: TextField = _

  @FXML
  protected var proxyField: TextField = _

  @FXML
  protected var sslTrustField: CheckBox = _

  @FXML
  protected var sslErrorAskField: CheckBox = _

  @FXML
  protected var maxErrorsField: TextField = _

  @FXML
  protected var attemptDelayField: TextField = _

  @FXML
  protected var cnxRequestTimeoutField: TextField = _

  @FXML
  protected var cnxTimeoutField: TextField = _

  @FXML
  protected var socketTimeoutField: TextField = _

  @FXML
  protected var idleTimeoutField: TextField = _

  @FXML
  protected var bufferMinSizeField: TextField = _

  @FXML
  protected var bufferMaxSizeField: TextField = _

  @FXML
  protected var sitesTab: Tab = _

  @FXML
  protected var siteRemoveButton: Button = _

  @FXML
  protected var sitesField: ListView[Option[SiteSettingsSnapshot]] = _

  @FXML
  protected var siteNameField: TextField = _

  @FXML
  protected var siteSslTrustField: CheckBox = _

  @FXML
  protected var siteSslErrorAskField: CheckBox = _

  @FXML
  protected var siteMaxCnxField: TextField = _

  @FXML
  protected var siteMaxSegmentsField: TextField = _

  protected var stage: Stage = _

  private var sitesChanged = false

  // Note: sites specific settings (e.g. sslTrust) will be considered as
  // (possibly) changed if sites were changed. So no need to explicitly
  // set them to true along 'sitesChanged'.
  private var sslTrustChanged = false

  private var cnxLimitChanged = false

  private var cnxBufferChanged = false

  private val sitesSnapshots = new SiteSettingsSnapshots

  def initialize(dialog: Dialog[_], snapshot1: SettingsSnapshot, snapshot2: SettingsSnapshot): Unit = {
    stage = Stages.getStage(dialog)
    // Remove content padding. Default style is:
    //  .dialog-pane > .content { -fx-padding: 0.833em; }
    dialog.getDialogPane.getContent.setStyle("-fx-padding: 0;")
    Icons.setIcons(stage.getScene.getRoot)

    // Note: we need to tell the combobox how to display both the 'button' area
    // (what is shown as selected) and the content (list of choices).
    languageChoice.setButtonCell(new I18NLocaleCell)
    languageChoice.setCellFactory(_ ⇒ new I18NLocaleCell)

    val locales = I18N.locales.sortBy(_.displayName)
    languageChoice.setItems(FXCollections.observableList(locales.asJava))

    // Build snapshots
    snapshot1.add({
      val setting = I18N.setting
        val snap = SettingSnapshot(setting).setOnRefreshDraft {
          languageChoice.getSelectionModel.getSelectedItem.code
        }.setOnChange { localeCode ⇒
          I18N.setLocale(localeCode)
        }
        def draftToField(): Unit = locales.find(_.code == snap.draft.get).foreach(languageChoice.getSelectionModel.select)
        snap.draft.listen(draftToField())
        draftToField()
        snap
      })
    snapshot2.add(
      booleanSettingSnapshot(debugField, Main.settings.debug)
      , {
        val setting = Main.settings.downloadsPath
        val snap = SettingSnapshot.opt(setting).setOnRefreshDraft {
          Option(downloadFolderField.getText).filter(_.trim.nonEmpty).map(Paths.get(_))
        }
        def draftToField(): Unit = downloadFolderField.setText(snap.draft.get.map(_.toString).orNull)
        snap.draft.listen(draftToField())
        draftToField()
        snap
      }, {
        val setting = Main.settings.downloadsExtension
        val snap = SettingSnapshot.opt(setting).setOnRefreshDraft {
          // Note: empty extension is allowed (disables it), so don't filter out
          // empty string value.
          Option(fileExtensionField.getText)
        }
        def draftToField(): Unit = fileExtensionField.setText(snap.draft.get.orNull)
        snap.draft.listen(draftToField())
        draftToField()
        snap
      }, booleanSettingSnapshot(removeCompletedField, Main.settings.removeCompleted)
      , intSettingSnapshot(maxDownloadsField, Main.settings.downloadsMax, isCnxLimit = true)
      , intSettingSnapshot(maxCnxField, Main.settings.cnxMax, isCnxLimit = true)
      , intSettingSnapshot(maxServerCnxField, Main.settings.cnxServerMax, isCnxLimit = true)
      , intSettingSnapshot(maxSegmentsField, Main.settings.sitesDefault.segmentsMax, isCnxLimit = true)
      , bytesSettingSnapshot(minSegmentSizeField, Main.settings.segmentsMinSize, isCnxLimit = true)
      , booleanSettingSnapshot(preallocateField, Main.settings.preallocateEnabled)
      , booleanSettingSnapshot(preallocateZeroField, Main.settings.preallocateZero)
      , bytesSettingSnapshot(writeBufferSizeField, Main.settings.bufferWriteFlushSize)
      , {
        val setting = Main.settings.proxy
        val snap = SettingSnapshot.opt(setting).withDefault {
          setting.refOpt
        }.setOnRefreshDraft {
          Option(proxyField.getText).filter(_.trim.nonEmpty)
        }
        def draftToField(): Unit = proxyField.setText(snap.draft.get.orNull)
        snap.draft.listen(draftToField())
        draftToField()
        snap
      }, sslTrustOnChange(booleanSettingSnapshot(sslTrustField, Main.settings.sitesDefault.sslTrust))
      , sslTrustOnChange(booleanSettingSnapshot(sslErrorAskField, Main.settings.sitesDefault.sslErrorAsk))
      , intSettingSnapshot(maxErrorsField, Main.settings.errorMax)
      , durationSettingSnapshot(attemptDelayField, Main.settings.errorDelay)
      , durationSettingSnapshot(cnxRequestTimeoutField, Main.settings.connectionRequestTimeout)
      , durationSettingSnapshot(cnxTimeoutField, Main.settings.connectTimeout)
      , durationSettingSnapshot(socketTimeoutField, Main.settings.socketTimeout)
      , durationSettingSnapshot(idleTimeoutField, Main.settings.idleTimeout)
      , bytesSettingSnapshot(bufferMinSizeField, Main.settings.bufferReadMin)
      , {
        val snap = bytesSettingSnapshot(bufferMaxSizeField, Main.settings.bufferReadMax)
        snap.setOnChange { _ ⇒
          cnxBufferChanged = true
        }
        snap
      }
    )

    // Notes:
    // sitesField may contain 'Option', but being actually Java the selected
    // item may be null. Hence the use of wrap+flatten (Option(item).flatten)
    // to get our expected Option.
    //
    // Since we may alter a site value then select another site, we need to take
    // care of various situations. Upon selecting another site we wish to:
    //  - keep modified value if valid
    //  - ignore modified value (and retain current draft) if invalid
    // We also wish for the 'Reset'/'Defaults' buttons to work as expected.
    // So we need to remember site settings snapshots in order to refresh the
    // draft value when applicable: sitesField items do hold both the settings
    // and associated snapshots.
    //
    // User-defined sites have optional values: we should be able to empty the
    // value so that it is treated as non-existent (where applicable we will
    // fallback to global settings).
    // On the contrary, the 'default' site needs to be special:
    //  - listed first, separated from others
    //  - cannot be removed
    //  - have 'real' default values, that cannot be emptied
    sitesField.setCellFactory { _ ⇒
      new ListCell[Option[SiteSettingsSnapshot]] with CellWithSeparator[SiteSettingsSnapshot] {
        override protected def itemText(item: SiteSettingsSnapshot): String = item.site
      }
    }
    val snaps = (Main.settings.sitesDefault :: Main.settings.getSites.values.toList).map(SiteSettingsSnapshot)
    sitesSnapshots.add(snaps)
    snapshot2.add(sitesSnapshots)

    sitesField.getSelectionModel.selectedItemProperty.listen { (_, oldItemOpt, _) ⇒
      // Refresh draft of previously selected site if any.
      // Note: right now, the selected item has already changed, so we cannot
      // rely on the 'standard' snapshot refreshing value.
      Option(oldItemOpt).flatten.foreach(_.refresh())
      updateSite()
    }
    sitesSnapshots.refreshSites(selectDefault = true)

    // We will have 4 buttons:
    //  - 'Reset': resets values to initial value
    //  - 'Defaults': sets values to defaults
    //  - 'Ok': apply changes
    //  - 'Cancel': do not apply changes
    val buttonTypeReset = new ButtonType(Strings.reset)
    val buttonTypeDefaults = new ButtonType(Strings.defaults)
    dialog.getDialogPane.getButtonTypes.addAll(buttonTypeReset, buttonTypeDefaults, ButtonType.OK, ButtonType.CANCEL)

    val buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)

    // 'Reset' and 'Defaults' shall change displayed values instead of closing
    // dialog.
    val buttonReset = dialog.getDialogPane.lookupButton(buttonTypeReset)
    buttonReset.addEventFilter(ActionEvent.ACTION, (event: ActionEvent) ⇒ {
      event.consume()
      snapshot1.resetDraft()
      snapshot2.resetDraft()
    })

    val buttonDefaults = dialog.getDialogPane.lookupButton(buttonTypeDefaults)
    buttonDefaults.addEventFilter(ActionEvent.ACTION, (event: ActionEvent) ⇒ {
      event.consume()
      snapshot1.resetDraft(original = false)
      snapshot2.resetDraft(original = false)
    })

    // Disable buttons when applicable.
    // Follow all fields containing values.
    // Also follow sites selection: if a value is invalid, it is not applied,
    // and if new selection has this value (and no other field change), we need
    // to make sure the field become valid again.
    // To cover the case where only a non-selected site setting changes upon
    // reset, we don't need to directly follow changes in sitesField items
    // because our changes (re-set all items) trigger clearing the selection
    // which empties the text fields (at least the site name was non-empty)
    // which are already followed.
    val dependencies = List(
      languageChoice.getSelectionModel.selectedItemProperty, debugField.selectedProperty,
      downloadFolderField.textProperty, fileExtensionField.textProperty, removeCompletedField.selectedProperty,
      maxDownloadsField.textProperty, maxCnxField.textProperty, maxServerCnxField.textProperty, maxSegmentsField.textProperty, minSegmentSizeField.textProperty,
      preallocateField.selectedProperty, preallocateZeroField.selectedProperty, writeBufferSizeField.textProperty,
      proxyField.textProperty, sslTrustField.selectedProperty, sslTrustField.indeterminateProperty,
      sslErrorAskField.selectedProperty, sslErrorAskField.indeterminateProperty,
      maxErrorsField.textProperty, attemptDelayField.textProperty,
      cnxRequestTimeoutField.textProperty, cnxTimeoutField.textProperty, socketTimeoutField.textProperty, idleTimeoutField.textProperty,
      bufferMinSizeField.textProperty, bufferMaxSizeField.textProperty,
      siteNameField.textProperty, siteSslTrustField.selectedProperty, siteSslTrustField.indeterminateProperty,
      siteSslErrorAskField.selectedProperty, siteSslErrorAskField.indeterminateProperty,
      siteMaxCnxField.textProperty, siteMaxSegmentsField.textProperty,
      sitesField.getSelectionModel.selectedItemProperty
    )
    new BindingsEx.Builder().add(buttonOk.disableProperty) {
      // Note: call 'checkForm' first to refresh errors if any
      !checkForm() || (!snapshot1.isDraftChanged() && !snapshot2.isDraftChanged())
    }.add(buttonReset.disableProperty) {
      !snapshot1.isDraftChanged() && !snapshot2.isDraftChanged()
    }.add(buttonDefaults.disableProperty) {
      !snapshot1.isDraftChanged(original = false) && !snapshot2.isDraftChanged(original = false)
    }.bind(dependencies)
    ()
  }

  def display(display: Display): Unit = {
    def findSiteEntry(site: String): Option[Option[SiteSettingsSnapshot]] = {
      sitesField.getItems.asScala.find { item ⇒
        item.exists(_.settings.site == site)
      }
    }

    display.serverSettings.foreach { server ⇒
      sitesTab.getTabPane.getSelectionModel.select(sitesTab)
      findSiteEntry(server) match {
        case Some(found) ⇒
          // A site already exists for this server
          sitesField.getSelectionModel.select(found)

        case None ⇒
          // Prepare a new site entry for this server
          sitesSnapshots.addSite(Some(server))
      }
    }
    display.siteSettings.foreach { siteSettings ⇒
      // 'default' site uses default settings (from the Main tab)
      if (!siteSettings.isDefault) {
        // Select 'Sites' tab and target site if found.
        findSiteEntry(siteSettings.site).foreach { found ⇒
          sitesTab.getTabPane.getSelectionModel.select(sitesTab)
          sitesField.getSelectionModel.select(found)
        }
      }
    }
  }

  /** Restores (persisted) view. */
  override protected def restoreView(): Unit = {
    // We will need to compute the minimum size of the dialog. It takes into
    // account the header, button bar and content (tab pane).
    // Tab pane skin is used to compute the tab pane minimum size, but none of
    // its children are managed, which gives a computed minimum size of 0 ...
    // Unlike in 'initialize', here is the first time the skin is present, so
    // make all children managed.
    val tabPane = sitesTab.getTabPane
    tabPane.getSkin.asInstanceOf[SkinBase[_]].getChildren.asScala.foreach { child ⇒
      child.setManaged(true)
    }

    super.restoreView()
  }

  def onDownloadFolderSelect(@unused event: ActionEvent): Unit = {
    val directoryChooser = new DirectoryChooser()
    PathChoosers.setInitialPath(directoryChooser, Option(downloadFolderField.getText).map(Paths.get(_).toFile).orNull)
    Option(directoryChooser.showDialog(stage)).foreach { selectedFolder ⇒
      downloadFolderField.setText(selectedFolder.toString)
    }
  }

  def onSiteAdd(@unused event: ActionEvent): Unit = {
    sitesSnapshots.addSite(None)
  }

  def onSiteRemove(@unused event: ActionEvent): Unit = {
    getSelectedSite.filterNot(_.isDefault).foreach { snap ⇒
      sitesSnapshots.removeSite(snap)
    }
  }

  private def updateSite(): Unit = {
    // Some 'default' site properties are also accessible in the other tabs.
    // We wish both fields to remain synced, so when applicable we create a
    // bidirectional binding (which first applies the second property value
    // to the first one).
    // Before any change, unbind the fields (noop if not bound).
    def unbind(dst: CheckBox, src: CheckBox): Unit = {
      dst.selectedProperty.unbindBidirectional(src.selectedProperty())
      dst.indeterminateProperty.unbindBidirectional(src.indeterminateProperty())
    }
    def bind(dst: CheckBox, src: CheckBox): Unit = {
      dst.selectedProperty.bindBidirectional(src.selectedProperty())
      dst.indeterminateProperty.bindBidirectional(src.indeterminateProperty())
    }
    unbind(siteSslTrustField, sslTrustField)
    unbind(siteSslErrorAskField, sslErrorAskField)
    siteMaxSegmentsField.textProperty.unbindBidirectional(maxSegmentsField.textProperty())
    Option(sitesField.getSelectionModel.getSelectedItem).flatten match {
      case Some(item) ⇒
        // Notes:
        // We need to get the current draft value (which may have been changed
        // - refreshed - previously) instead of the setting value (draft not
        // yet applied).
        siteMaxCnxField.setText(item.cnxMax.getDraftValue(refreshed = false).map(_.toString).orNull)
        if (item.isDefault) {
          bind(siteSslTrustField, sslTrustField)
          bind(siteSslErrorAskField, sslErrorAskField)
          siteMaxSegmentsField.textProperty.bindBidirectional(maxSegmentsField.textProperty())
        } else {
          optToField(item.sslTrust.getDraftValue(refreshed = false), siteSslTrustField)
          optToField(item.sslErrorAsk.getDraftValue(refreshed = false), siteSslErrorAskField)
          siteMaxSegmentsField.setText(item.segmentsMax.getDraftValue(refreshed = false).map(_.toString).orNull)
        }
        siteNameField.setText(item.site)
        siteNameField.setDisable(item.isDefault)
        siteRemoveButton.setDisable(item.isDefault)

      case None ⇒
        siteMaxCnxField.setText(null)
        siteMaxSegmentsField.setText(null)
        siteNameField.setText(null)
        siteNameField.setDisable(false)
        siteRemoveButton.setDisable(true)
    }
  }

  private def sslTrustOnChange(snap: ConfigOptEntrySnapshot[Boolean]): ConfigOptEntrySnapshot[Boolean] = {
    snap.setOnChange { _ ⇒
      sslTrustChanged = true
    }
  }

  private def tryConnectionOnChange(snap: SettingSnapshot[_]): Unit = {
    snap.setOnChange { _ ⇒
      cnxLimitChanged = true
    }
  }

  private def booleanSettingSnapshot(field: CheckBox, setting: ConfigEntry[Boolean],
    siteSettings: Option[Settings#SiteSettings] = None): ConfigOptEntrySnapshot[Boolean] =
  {
    // We have to handle standard settings and site settings.
    // For the later, there is a need to determine (in callbacks) whether the
    // target site is selected.
    @inline
    def isSelected: Boolean = siteSettings.forall(isSiteSelected)
    val snap = SettingSnapshot.opt(setting)
    snap.setOnRefreshDraft {
      // Notes:
      // We are called in two cases:
      //  1. To get the 'refreshed' draft value
      //  2. To refresh the current draft value itself
      // 1. matters to have 'Reset'/'Defaults'/'Ok' buttons enabled when
      // applicable.
      // We make sure 2. is only actually called upon saving changes in
      // configuration, which should only be possible if at least one value was
      // changed and all values are valid (checkForm also matters for 'Ok').
      // If our site is not selected we keep the current draft value.
      if (isSelected) fieldToOpt(field)
      else snap.getDraftValue(refreshed = false)
    }
    def draftToField(): Unit = if (isSelected) optToField(snap.draft.get, field)
    snap.draft.listen(draftToField())
    draftToField()
    snap
  }

  private def intSettingSnapshot(field: TextField, setting: ConfigEntry[Int],
    siteSettings: Option[Settings#SiteSettings] = None,
    isCnxLimit: Boolean = false): ConfigOptEntrySnapshot[Int] =
  {
    @inline
    def isSelected: Boolean = siteSettings.forall(isSiteSelected)
    val snap = SettingSnapshot.opt(setting)
    snap.setOnRefreshDraft {
      // Here we wish to use None when either:
      //  - value is not an integer: invalid value
      //  - value is empty:
      //   -> for non-default sites this is allowed (to remove the config entry)
      //   -> for default site, this is an invalid value
      //   -> for other settings, this is also an invalid value
      if (isSelected) getInt(field.getText)
      else snap.getDraftValue(refreshed = false)
    }
    if (isCnxLimit) tryConnectionOnChange(snap)
    def draftToField(): Unit = if (isSelected) field.setText(snap.draft.get.map(_.toString).orNull)
    snap.draft.listen(draftToField())
    draftToField()
    snap
  }

  private def bytesSettingSnapshot(field: TextField, setting: ConfigEntry[Long], isCnxLimit: Boolean = false): SettingSnapshot[Long] = {
    val snap = SettingSnapshot(setting)
    snap.setOnRefreshDraft {
      val raw = field.getText
      snap.setRawDraft(raw)
      getBytes(raw).getOrElse(-1)
    }
    if (isCnxLimit) tryConnectionOnChange(snap)
    def draftToField(): Unit = field.setText(snap.rawDraft.get.unwrapped.toString)
    snap.rawDraft.listen(draftToField())
    draftToField()
    snap
  }

  private def durationSettingSnapshot(field: TextField, setting: ConfigEntry[FiniteDuration]): SettingSnapshot[FiniteDuration] = {
    val snap = SettingSnapshot(setting)
    snap.setOnRefreshDraft {
      val raw = field.getText
      snap.setRawDraft(raw)
      getDuration(raw).getOrElse(-1.millis)
    }
    def draftToField(): Unit = field.setText(snap.rawDraft.get.unwrapped.toString)
    snap.rawDraft.listen(draftToField())
    draftToField()
    snap
  }

  private def checkForm(): Boolean = {
    val maxDownloadsOk = getInt(maxDownloadsField.getText).getOrElse(-1) > 0
    val maxCnxOk = getInt(maxCnxField.getText).getOrElse(-1) > 0
    val maxServerCnxOk = getInt(maxServerCnxField.getText).getOrElse(-1) > 0
    val maxSegmentsOk = getInt(maxSegmentsField.getText).getOrElse(-1) > 0
    val minSegmentSizeOk = getBytes(minSegmentSizeField.getText).getOrElse(-1L) > 0
    val writeBufferSizeOk = getBytes(writeBufferSizeField.getText).getOrElse(-1L) > 0
    val maxErrorsOk = getInt(maxErrorsField.getText).getOrElse(-1) > 0
    val attemptDelayOk = getDuration(attemptDelayField.getText).getOrElse(-1.millis).length >= 0
    val cnxRequestTimeoutOk = getDuration(cnxRequestTimeoutField.getText).getOrElse(-1.millis).length >= 0
    val cnxTimeoutOk = getDuration(cnxTimeoutField.getText).getOrElse(-1.millis).length >= 0
    val socketTimeoutOk = getDuration(socketTimeoutField.getText).getOrElse(-1.millis).length >= 0
    val idleTimeoutOk = getDuration(idleTimeoutField.getText).getOrElse(-1.millis).length >= 0
    val bufferMinSizeOk = getBytes(bufferMinSizeField.getText).getOrElse(-1L) > 0
    val bufferMaxSizeOk = getBytes(bufferMaxSizeField.getText).getOrElse(-1L) >= 0

    val isDefaultSite = getSelectedSite.exists(_.isDefault)
    val siteNameOk = getSiteName.exists { site ⇒
      getSelectedSite.map(_.site).contains(site) ||
        !sitesSnapshots.getSnapshots.map(_.site).toSet.contains(site)
    }
    // We allow emptying non-default site values
    val siteMaxCnxOk = getInt(siteMaxCnxField.getText).getOrElse {
      if (isDefaultSite || !isEmpty(siteMaxCnxField.getText)) -1
      else 1
    } > 0
    val siteMaxSegmentsOk = getInt(siteMaxSegmentsField.getText).getOrElse {
      if (isDefaultSite || !isEmpty(siteMaxSegmentsField.getText)) -1
      else 1
    } > 0

    Styles.toggleError(maxDownloadsField, !maxDownloadsOk, Strings.positiveValueExpected)
    Styles.toggleError(maxCnxField, !maxCnxOk, Strings.positiveValueExpected)
    Styles.toggleError(maxServerCnxField, !maxServerCnxOk, Strings.positiveValueExpected)
    Styles.toggleError(maxSegmentsField, !maxSegmentsOk, Strings.positiveValueExpected)
    Styles.toggleError(minSegmentSizeField, !minSegmentSizeOk, Strings.validSizeExpected)
    Styles.toggleError(writeBufferSizeField, !writeBufferSizeOk, Strings.validSizeExpected)
    Styles.toggleError(maxErrorsField, !maxErrorsOk, Strings.positiveValueExpected)
    Styles.toggleError(attemptDelayField, !attemptDelayOk, Strings.validDurationExpected)
    Styles.toggleError(cnxRequestTimeoutField, !cnxRequestTimeoutOk, Strings.validDurationExpected)
    Styles.toggleError(cnxTimeoutField, !cnxTimeoutOk, Strings.validDurationExpected)
    Styles.toggleError(socketTimeoutField, !socketTimeoutOk, Strings.validDurationExpected)
    Styles.toggleError(idleTimeoutField, !idleTimeoutOk, Strings.validDurationExpected)
    Styles.toggleError(bufferMinSizeField, !bufferMinSizeOk, Strings.validSizeExpected)
    Styles.toggleError(bufferMaxSizeField, !bufferMaxSizeOk, Strings.validSizeExpected)
    Styles.toggleError(siteNameField, !siteNameOk, Strings.mustNonEmptyUnique)
    Styles.toggleError(siteMaxCnxField, !siteMaxCnxOk, Strings.positiveValueExpected)
    Styles.toggleError(siteMaxSegmentsField, !siteMaxSegmentsOk, Strings.positiveValueExpected)

    maxDownloadsOk && maxCnxOk && maxServerCnxOk &&
      maxSegmentsOk && minSegmentSizeOk && writeBufferSizeOk &&
      maxErrorsOk && attemptDelayOk && cnxRequestTimeoutOk && cnxTimeoutOk && socketTimeoutOk && idleTimeoutOk &&
      bufferMinSizeOk && bufferMaxSizeOk &&
      siteNameOk && siteMaxCnxOk && siteMaxSegmentsOk
  }

  private def optToField(v: Option[Boolean], field: CheckBox): Unit = {
    if (field.isAllowIndeterminate) {
      v match {
        case Some(b) ⇒
          field.setIndeterminate(false)
          field.setSelected(b)

        case None ⇒
          field.setIndeterminate(true)
      }
    } else {
      // Since 'indeterminate' state is not allowed, we use 'false' as default
      // value.
      field.setSelected(v.getOrElse(false))
    }
  }

  private def fieldToOpt(field: CheckBox): Option[Boolean] = {
    if (field.isAllowIndeterminate && field.isIndeterminate) None
    else Some(field.isSelected)
  }

  private def isEmpty(s: String): Boolean = Option(s).forall(_.trim.isEmpty)

  private def getSelectedSite: Option[SiteSettingsSnapshot] =
    Option(sitesField.getSelectionModel.getSelectedItem).flatten

  private def isSiteSelected(settings: Settings#SiteSettings): Boolean = {
    getSelectedSite.map(_.settings).contains(settings)
  }

  private def getSiteName: Option[String] = {
    Option(siteNameField.getText).map(_.trim.toLowerCase).filter(_.nonEmpty)
  }

  private def getInt(s: String): Option[Int] = {
    Option(s).flatMap { v ⇒
      try { Some(v.toInt) } catch { case _: Exception ⇒ None }
    }
  }

  private def getBytes(s: String): Option[Long] = {
    Option(s).flatMap { v ⇒
      try { Some(Units.storage.fromHumanReadable(v)) } catch { case _: Exception ⇒ None }
    }
  }

  private def getDuration(s: String): Option[FiniteDuration] = {
    Option(s).flatMap { v ⇒
      try {
        Util.parseDuration(v) match {
          case d: FiniteDuration ⇒ Some(d)
          case _ ⇒ None
        }
      } catch { case _: Exception ⇒ None }
    }
  }

  protected case class SiteSettingsSnapshot(settings: Main.settings.SiteSettings) extends SettingsSnapshot {

    // Current ('draft') site name.
    // Initially is the original site name.
    var site: String = settings.site

    val isDefault: Boolean = settings.isDefault

    val sslTrust: ConfigOptEntrySnapshot[Boolean] =
      sslTrustOnChange(booleanSettingSnapshot(siteSslTrustField, settings.sslTrust, Some(settings)))
    val sslErrorAsk: ConfigOptEntrySnapshot[Boolean] =
      sslTrustOnChange(booleanSettingSnapshot(siteSslErrorAskField, settings.sslErrorAsk, Some(settings)))
    val cnxMax: ConfigOptEntrySnapshot[Int] =
      intSettingSnapshot(siteMaxCnxField, settings.cnxMax, Some(settings), isCnxLimit = true)
    val segmentsMax: ConfigOptEntrySnapshot[Int] =
      intSettingSnapshot(siteMaxSegmentsField, settings.segmentsMax, Some(settings), isCnxLimit = true)

    // Add the managed snapshots
    add(sslTrust, sslErrorAsk, cnxMax, segmentsMax)

    def isSiteChanged: Boolean = refreshedSiteName(checkSelected = true) != settings.site

    def refreshDraftBoolean(field: CheckBox, snap: ConfigOptEntrySnapshot[Boolean]): Unit = {
      snap.draft.set(fieldToOpt(field))
    }

    def refreshDraftInt(field: TextField, snap: ConfigOptEntrySnapshot[Int]): Unit = {
      // We are called to refresh the draft value because the selection moved
      // from our site to another.
      // We only allow positive values.
      // If value is invalid, we keep the current draft value. The only
      // exception being non-default site value that can be emptied, in which
      // case we can use None (to 'reset' entry).
      val text = field.getText
      snap.draft.set {
        getInt(text).filter(_ > 0).orElse {
          if (!isDefault && isEmpty(text)) None
          else snap.getDraftValue(refreshed = false)
        }
      }
    }

    // Refresh snapshots (drafts) upon selection change
    def refresh(): Unit = {
      val name = refreshedSiteName(checkSelected = false)
      val siteNameChanged = site != name
      site = name
      refreshDraftBoolean(siteSslTrustField, sslTrust)
      refreshDraftBoolean(siteSslErrorAskField, sslErrorAsk)
      refreshDraftInt(siteMaxCnxField, cnxMax)
      refreshDraftInt(siteMaxSegmentsField, segmentsMax)
      if (siteNameChanged) {
        // Refresh sites if this site name changed.
        // Since we are called upon a selection change, we queue refreshing
        // for later.
        JFXSystem.runLater {
          sitesSnapshots.refreshSites(reselect = true)
        }
      }
    }

    private def refreshedSiteName(checkSelected: Boolean): String = {
      if (!checkSelected || isSiteSelected(settings)) {
        getSiteName.filter { name ⇒
          (site == name) ||
            !sitesSnapshots.getSnapshots.map(_.site).toSet.contains(name)
        }.getOrElse(site)
      } else site
    }

    override def isDraftChanged(original: Boolean, refreshed: Boolean): Boolean = {
      // There is no 'default' value for non-default sites
      (isDefault || original) && {
        (refreshedSiteName(checkSelected = true) != settings.site) || super.isDraftChanged(original, refreshed)
      }
    }

    override def applyDraft(): Boolean = {
      def optToValue[A](snap: SettingSnapshot[Option[A]], configEntry: ConfigEntry[A]): Unit = {
        snap.getDraftValue() match {
          case Some(v) ⇒ configEntry.set(v)
          case None ⇒ configEntry.reset()
        }
      }
      // Refresh site name if applicable
      site = refreshedSiteName(checkSelected = true)
      // If site was renamed, we need to make sure the 'new' entry is created.
      // Parent did remove all 'old' entries.
      // If site was not renamed, simply apply draft changes.
      if (isSiteChanged) {
        // Create a brand new site settings entry
        val newSettings = Main.settings.getSite(site, allowDefault = false)
        // And set our values. Make sure to get the refreshed draft value and
        // remove entry when applicable.
        optToValue(sslTrust, newSettings.sslTrust)
        optToValue(sslErrorAsk, newSettings.sslErrorAsk)
        optToValue(cnxMax, newSettings.cnxMax)
        optToValue(segmentsMax, newSettings.segmentsMax)

        sitesChanged = true

        true
      } else super.applyDraft()
    }

    override def resetDraft(original: Boolean, refresh: Boolean): Unit = {
      // There is no 'default' value for non-default sites
      if (isDefault || original) super.resetDraft(original, refresh)
    }

  }

  private class SiteSettingsSnapshots extends Snapshots[SiteSettingsSnapshot] {

    private var removed: Set[SiteSettingsSnapshot] = Set.empty

    def addSite(siteOpt: Option[String]): Unit = {
      // Create a new snapshot for this site
      @scala.annotation.tailrec
      def getName(idx: Long): String = {
        val name = s"site${if (idx > 0) idx else ""}.ext"
        if (snapshots.map(_.site).toSet.contains(name)) getName(idx + 1)
        else name
      }
      // Use an empty original name: isDraftChanged will automatically consider
      // this snapshot changed, and we can easily distinguish newly-added
      // entries.
      val siteSettings = new Main.settings.SiteSettings("")
      val snap = SiteSettingsSnapshot(siteSettings)
      // Build a unique site name to apply.
      snap.site = siteOpt.getOrElse(getName(0))
      sitesField.getItems.get(0) match {
        case Some(sitesDefault) ⇒
          // Use current 'default' site snapshot values.
          snap.sslTrust.draft.set(sitesDefault.sslTrust.getDraftValue())
          snap.sslErrorAsk.draft.set(sitesDefault.sslErrorAsk.getDraftValue())
          snap.cnxMax.draft.set(sitesDefault.cnxMax.getDraftValue())
          snap.segmentsMax.draft.set(sitesDefault.segmentsMax.getDraftValue())

        case None ⇒
          // No 'default' site snapshot (should not happen)
          snap.sslTrust.draft.set(Main.settings.sitesDefault.sslTrust.opt)
          snap.sslErrorAsk.draft.set(Main.settings.sitesDefault.sslErrorAsk.opt)
          snap.cnxMax.draft.set(Main.settings.sitesDefault.cnxMax.opt)
          snap.segmentsMax.draft.set(Main.settings.sitesDefault.segmentsMax.opt)
      }
      // Add to our known list of settings
      add(snap)
      // Refresh sites (re-ordering may be needed)
      refreshSites()
      // Select this newly added site
      sitesField.getSelectionModel.select(Some(snap))
    }

    def removeSite(snap: SiteSettingsSnapshot): Unit = {
      // Remember removed site (to re-add it if necessary).
      // We don't care about newly added sites (empty original name).
      if (snap.settings.site.nonEmpty) removed += snap
      // Remove from our known list of settings
      snapshots = snapshots.filterNot(_ eq snap)
      // Remove site from list. No need to refresh/re-order in this case.
      val idx0 = sitesField.getSelectionModel.getSelectedIndex
      val items = sitesField.getItems
      items.remove(Some(snap))
      // Re-select next site (or default when applicable)
      val idx =
        if (idx0 < items.size) idx0
        else if (idx0 == 2) 0
        else idx0 - 1
      sitesField.getSelectionModel.select(idx)
    }

    override def isDraftChanged(original: Boolean, refreshed: Boolean): Boolean = {
      removed.nonEmpty || super.isDraftChanged(original, refreshed)
    }

    override def applyDraft(): Boolean = {
      // First remove sites; also concerns 'old' name of renamed sites.
      // We need to do this first (for all entries) before (re-)adding remaining
      // sites to take care of all situations - e.g. the edge case of two sites
      // entries being 'swapped' (renamed to each other).
      removed.foreach { snap ⇒
        Main.settings.removeSite(snap.settings)
        sitesChanged = true
      }
      removed = Set.empty
      snapshots.filter { snap ⇒
        snap.settings.site.nonEmpty && snap.isSiteChanged
      }.foreach { snap ⇒
        Main.settings.removeSite(snap.settings.site)
        // Note: here site was renamed, and cnxLimitChanged will be set through
        // super.applyDraft (which also takes care of 'new' sites).
      }
      super.applyDraft()
    }

    override def resetDraft(original: Boolean, refresh: Boolean): Unit = {
      // Remove added sites (original name empty)
      snapshots = snapshots.filterNot(_.settings.site.isEmpty)
      // Re-add removed sites
      add(removed.toSeq)
      removed = Set.empty
      // Reset any site name change
      snapshots.foreach { snap ⇒
        snap.site = snap.settings.site
      }
      // Now let generic 'reset' take care of settings
      super.resetDraft(original, refresh)
      // And finally refresh sites. Do it after super.resetDraft so that (as a
      // side-effect) it makes the button states recomputed.
      refreshSites(reset = true, reselect = true)
    }

    def refreshSites(reset: Boolean = false, selectDefault: Boolean = false, reselect: Boolean = false): Unit = {
      val selected = sitesField.getSelectionModel.getSelectedItem
      // Reset fields if needed
      if (reset) updateSite()
      // Re-order sites
      // Always keep default site first.
      snapshots = snapshots.filter(_.isDefault) ::: snapshots.filterNot(_.isDefault).sortBy(_.site)
      // Refresh list
      val entries = List(Some(snapshots.head), None) ::: snapshots.tail.map(Some(_))
      sitesField.getItems.setAll(entries:_*)
      if (selectDefault) sitesField.getSelectionModel.select(0)
      else if (reselect) {
        // Try to re-select entry. Fallback to default if entry is missing.
        if (sitesField.getItems.contains(selected)) sitesField.getSelectionModel.select(selected)
        else sitesField.getSelectionModel.select(0)
      }
    }

  }

}

object OptionsController {

  import I18N.Strings

  private val settingsKeyPrefix = "options"

  private val stageLocation = ConfigEntry.from[StageLocation](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_DL_MNGR, Settings.KEY_STAGE, settingsKeyPrefix, "location")

  // What to specifically display in the options
  case class Display(
    serverSettings: Option[String] = None,
    siteSettings: Option[Main.settings.SiteSettings] = None
  )

  case class Result(
    sitesChanged: Boolean = false,
    sslTrustChanged: Boolean = false,
    cnxLimitChanged: Boolean = false,
    cnxBufferChanged: Boolean = false,
    reload: Boolean = false
  )

  def buildDialog(owner: Window, display: Display): Dialog[Result] = {
    val dialog = new Dialog[Result]()
    Stages.initOwner(dialog, owner)
    Stages.getStage(dialog).getIcons.clear()
    List(256.0, 128.0, 64.0, 32.0, 16.0).foreach { size ⇒
      val icon = Icons.cog(targetSvgSize = size)
      // We want to apply CSS, but for it to work properly there must be a
      // "root" element (which holds some JavaFX CSS variables).
      icon.pane.getStyleClass.add("root")
      icon.pane.getStylesheets.add(getClass.getResource("/css/main.css").toExternalForm)
      Stages.getStage(dialog).getIcons.add(Graphics.buildImage(icon.pane))
    }
    dialog.setTitle(Strings.options)

    // Notes: snapshot is used to check whether something changed.
    // The controller does not actually apply changes until asked to.
    // Values are separated in two groups depending on whether caller needs to
    // reload (view) after changes.
    val snapshot1 = new SettingsSnapshot()
    val snapshot2 = new SettingsSnapshot()

    val loader = new FXMLLoader(getClass.getResource("/fxml/options.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load[Node]())
    val controller = loader.getController[OptionsController]
    controller.initialize(dialog, snapshot1, snapshot2)
    controller.display(display)

    Dialogs.addPersistence(dialog, controller)

    dialog.setResultConverter(resultConverter(controller, snapshot1, snapshot2) _)

    dialog
  }

  def resultConverter(controller: OptionsController,
    snapshot1: SettingsSnapshot, snapshot2: SettingsSnapshot)(buttonType: ButtonType): Result =
  {
    if (buttonType != ButtonType.OK) Result()
    else {
      // Apply changes
      val r = Main.settings.settings.delayedSave {
        snapshot2.applyDraft()
        snapshot1.applyDraft()
      }

      // If sites changed (added/removed), sites settings may also have changed
      // (an existing download may now be associated to another site).
      Result(
        sitesChanged = controller.sitesChanged,
        sslTrustChanged = controller.sslTrustChanged || controller.sitesChanged,
        cnxLimitChanged = controller.cnxLimitChanged || controller.sitesChanged,
        cnxBufferChanged = controller.cnxBufferChanged,
        reload = r
      )
    }
  }

}
