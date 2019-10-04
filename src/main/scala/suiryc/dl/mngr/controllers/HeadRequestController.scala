package suiryc.dl.mngr.controllers

import java.net.URI
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Node
import javafx.scene.control._
import javafx.stage.Window
import org.apache.http.{HttpEntity, HttpResponse}
import org.apache.http.client.methods.HttpRequestBase
import org.apache.http.client.protocol.HttpClientContext
import org.apache.http.client.utils.URIUtils
import org.apache.http.concurrent.FutureCallback
import org.apache.http.entity.ContentType
import org.apache.http.nio.{ContentDecoder, IOControl}
import org.apache.http.nio.protocol.{AbstractAsyncResponseConsumer, BasicAsyncRequestProducer}
import org.apache.http.protocol.HttpContext
import suiryc.dl.mngr.model.DownloadException
import suiryc.dl.mngr.{DownloadManager, I18N, Main}
import suiryc.dl.mngr.util.{Http, Icons}
import suiryc.scala.io.PathsEx
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.scene.Graphics
import suiryc.scala.javafx.scene.control.Dialogs
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.misc.Units

class HeadRequestController {

  import HeadRequestController._
  import I18N.Strings

  @FXML
  protected var dlURIField: TextField = _

  @FXML
  protected var dlSizeLabel: Label = _

  @FXML
  protected var dlFileField: TextField = _

  @FXML
  protected var dlLastModifiedLabel: Label = _

  private var hints = Option.empty[DownloadHints]

  def initialize(dialog: Dialog[_], dlMngr: DownloadManager, request: HttpRequestBase): Unit = {
    import scala.jdk.CollectionConverters._
    val stage = Stages.getStage(dialog)
    // Disable buttons until request is done.
    val buttons = dialog.getDialogPane.getButtonTypes.asScala.map(dialog.getDialogPane.lookupButton)
    buttons.foreach(_.setDisable(true))

    dlURIField.setText(request.getURI.toString)

    val requestProducer = new BasicAsyncRequestProducer(URIUtils.extractHost(request.getURI), request)
    // Attach a context so that we can retrieve redirection URIs.
    // It happens that redirection URIs are stored in the HTTP context.
    // An alternative would be to add an interceptor:
    //   - in the shared client, and use the context, possibly to execute a
    //     specific callback
    //  or
    //   - in a one-shot client, with a specific callback to execute
    val context = HttpClientContext.create()

    // Response consumer: we only care about the headers.
    val responseConsumer = new HeadResponseConsumer {
      override def onResponseReceived(response: HttpResponse): Unit = {
        val statusLine = response.getStatusLine
        if (statusLine.getStatusCode / 100 != 2) {
          // Request failed with HTTP code other than 2xx
          failed(DownloadException(s"Request failed with HTTP status=<(${statusLine.getStatusCode}) ${statusLine.getReasonPhrase}>"))
        } else {
          this.response = Option(response)
        }
      }
      override def onEntityEnclosed(entity: HttpEntity, contentType: ContentType): Unit = {}
      override def buildResult(context: HttpContext): Unit = {}
      override def releaseResources(): Unit = {}
      def onContentReceived(decoder: ContentDecoder, ioctrl: IOControl): Unit = {
        // We should not receive any content. If we do, shutdown the channel.
        ioctrl.shutdown()
      }
    }

    // Handle the actual response.
    def handleResponse(exOpt: Option[Exception]): Unit = JFXSystem.runLater {
      buttons.foreach(_.setDisable(false))
      responseConsumer.response match {
        case Some(response) =>
          val contentLength = Http.getContentLength(response)
          val lastModified = Http.getLastModified(response)
          val acceptRanges = Http.handleBytesRange(response)
          val filename = Http.getFilename(response).map { filename0 =>
            // Sanitize filename, and inform user if value changed.
            val filename = PathsEx.sanitizeFilename(Http.getFilename(filename0))
            if (filename != filename0) {
              Dialogs.information(
                owner = Option(stage),
                title = None,
                headerText = Some(s"${Strings.reservedChars}\n$filename0"),
                contentText = Some(filename)
              )
            }
            filename
          }
          val hintUri = Option(context.getRedirectLocations).flatMap { redirectLocations =>
            if (!redirectLocations.isEmpty) {
              // Keep original URI as tooltip ...
              dlURIField.setTooltip {
                new Tooltip(dlURIField.getText)
              }
              // ... ans show real (redirected) URI
              val redirected = redirectLocations.asScala.last
              dlURIField.setText(redirected.toString)
              Some(redirected)
            } else {
              None
            }
          }
          if (contentLength >= 0) {
            dlSizeLabel.setText {
              s"$contentLength (${Units.storage.toHumanReadable(contentLength)})"
            }
          }
          dlSizeLabel.setGraphic {
            val warnings =
              (if (contentLength >= 0) Nil else List(Strings.unknownSize)) :::
                (if (acceptRanges) Nil else List(Strings.resumeUnsupported))
            if (warnings.nonEmpty) {
              val icon = Icons.exclamationTriangle(styleClass = List("icon-exclamation-triangle-warning")).pane
              val tooltip = new Tooltip(warnings.mkString("\n"))
              Tooltip.install(icon, tooltip)
              icon
            } else null
          }
          dlFileField.setText(filename.orNull)
          dlLastModifiedLabel.setText(lastModified.map(MainController.dateFormatter.format).orNull)
          val hintSize = if (contentLength >= 0) Some(contentLength) else None
          hints = Some(DownloadHints(
            uri = hintUri,
            size = hintSize,
            filename = filename
          ))

        case None =>
          exOpt match {
            case Some(ex) =>
              // Failure
              Main.controller.displayError(
                title = None,
                contentText = Some(Strings.unexpectedIssue),
                ex = ex
              )
              dialog.close()

            case None =>
              // We assume the request was cancelled, so don't show anything.
              dialog.close()
          }
      }
    }

    // Execute the request and handle the response when available.
    val futureCallback = new FutureCallback[Unit] {
      override def completed(result: Unit): Unit = handleResponse(None)
      override def failed(ex: Exception): Unit = handleResponse(Option(ex))
      override def cancelled(): Unit = handleResponse(None)
    }
    val client = dlMngr.getClient(trustAll = true)
    client.execute(requestProducer, responseConsumer, context, futureCallback)
    ()
  }

}

object HeadRequestController {

  import I18N.Strings

  private case class Result(
    response: Option[HttpResponse] = None,
    ex: Option[Exception] = None
  )

  protected case class DownloadHints(
    uri: Option[URI],
    size: Option[Long],
    filename: Option[String]
  )

  private trait HeadResponseConsumer extends AbstractAsyncResponseConsumer[Unit] {
    var response = Option.empty[HttpResponse]
  }

  def buildDialog(owner: Window, dlMngr: DownloadManager, request: HttpRequestBase): Dialog[Option[DownloadHints]] = {
    val dialog = new Dialog[Option[DownloadHints]]()
    Stages.initOwner(dialog, owner)
    Stages.getStage(dialog).getIcons.clear()
    List(256.0, 128.0, 64.0, 32.0, 16.0).foreach { size =>
      val icon = Icons.bug(targetSvgSize = size)
      // We want to apply CSS, but for it to work properly there must be a
      // "root" element (which holds some JavaFX CSS variables).
      icon.pane.getStyleClass.add("root")
      icon.pane.getStylesheets.add(getClass.getResource("/css/main.css").toExternalForm)
      Stages.getStage(dialog).getIcons.add(Graphics.buildImage(icon.pane))
    }
    dialog.setTitle(Strings.properties)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.CLOSE, ButtonType.APPLY)

    val loader = new FXMLLoader(getClass.getResource("/fxml/request-head.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load[Node]())
    val controller = loader.getController[HeadRequestController]
    controller.initialize(dialog, dlMngr, request)

    dialog.setResultConverter(resultConverter(controller, request))

    dialog
  }

  def resultConverter(controller: HeadRequestController, request: HttpRequestBase)(buttonType: ButtonType): Option[DownloadHints] = {
    // Buttons are disabled until request is done, but dialog can still be
    // closed (same result as with 'Close' button), which allows aborting the
    // request if it does not complete fast enough.
    if (buttonType == ButtonType.CLOSE) {
      // Abort request. If it was completed, this does nothing. If it is still
      // ongoing, we better end it ASAP upon closing the dialog.
      request.abort()
      None
    } else {
      controller.hints
    }
  }

}
