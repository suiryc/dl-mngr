package suiryc.dl.mngr

import suiryc.scala.settings.ConfigEntry
import suiryc.scala.util.{I18NWithCache, I18NWithConfigEntry}

object I18N extends suiryc.scala.util.I18N("i18n.dl-mngr") with I18NWithConfigEntry with I18NWithCache {

  override val setting: ConfigEntry[String] = Main.settings.localeCode

  object Strings {
    def addDownload: String = getString("Add download")
    def cliIssue: String = getString("error.cli-issue")
    def defaults: String = getString("Defaults")
    def downloadAlreadyFile: String = getString("confirmation.download-already-file")
    def downloadAlreadyUri: String = getString("confirmation.download-already-uri")
    def downloaded: String = getString("Downloaded")
    def error: String = getString("Error")
    def eta: String = getString("ETA")
    def file: String = getString("File")
    def invalidURI: String = getString("error.invalid-uri")
    def kind: String = getString("Kind")
    def message: String = getString("Message")
    def moveDown: String = getString("Move down")
    def moveFirst: String = getString("Move first")
    def moveLast: String = getString("Move last")
    def moveUp: String = getString("Move up")
    def mustNonEmptyUnique: String = getString("error.must-non-empty-unique")
    def options: String = getString("Options")
    def positiveValueExpected: String = getString("Positive value expected")
    def progress: String = getString("Progress")
    def readIssue: String = getString("error.read-issue")
    def rename: String = getString("Rename")
    def renamedFile: String = getString("information.renamed-file")
    def reservedChars: String = getString("information.reserved-chars")
    def reset: String = getString("Reset")
    def restart: String = getString("Restart")
    def remove: String = getString("Remove")
    def removeCompleted: String = getString("Remove completed")
    def resume: String = getString("Resume")
    def resumeAll: String = getString("Resume all")
    def resumeUnsupported: String = getString("warning.resume-unsupported")
    def writeIssue: String = getString("error.write-issue")
    def segments: String = getString("Segments")
    def server: String = getString("Server")
    def site: String = getString("Site")
    def size: String = getString("Size")
    def speed: String = getString("Speed")
    def sslIssue: String = getString("error.ssl-issue")
    def sslTrust: String = getString("confirmation.ssl-trust")
    def stop: String = getString("Stop")
    def stopAll: String = getString("Stop all")
    def stopDlsOnRemove: String = getString("confirmation.downloads-remove-stop")
    def stopDlsOnExit: String = getString("confirmation.exit-running-downloads")
    def time: String = getString("Time")
    def unexpectedIssue: String = getString("error.unexpected-issue")
    def unknownSize: String = getString("warning.size-unknown")
    def validDurationExpected: String = getString("Valid duration expected")
    def validSizeExpected: String = getString("Valid size expected")
  }

}
