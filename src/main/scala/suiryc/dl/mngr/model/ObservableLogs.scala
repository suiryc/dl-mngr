package suiryc.dl.mngr.model

import java.time.LocalDateTime
import javafx.collections.{FXCollections, ObservableList}

trait ObservableLogs {

  /**
   * Logs.
   *
   * Notes:
   * FXCollections are sensible to concurrent changes when dealing with added
   * elements: the exposed sublist wraps the original collection, and iterating
   * over it may throw a ConcurrentModificationException if the collection has
   * been changed since the exposed change.
   * This means the bare minimum is for both modifications and listeners to
   * either work in the same thread or synchronize themselves. The easiest
   * solution is to first synchronize 'addLog' (prevent concurrent modifications
   * on this end) and make sure listeners work on changes in the caller thread.
   */
  val logs: ObservableList[LogEntry] = FXCollections.observableArrayList()

  /**
   * Do something with logs.
   *
   * Execute code while synchronized on logs.
   */
  def withLogs[A](f: ObservableList[LogEntry] => A): A = logs.synchronized {
    f(logs)
  }

  def addLog(kind: LogKind.Value, message: String, exOpt: Option[Throwable] = None): Unit = logs.synchronized {
    val entry = LogEntry(time = LocalDateTime.now, kind = kind, message = message, exOpt = exOpt)
    logs.add(entry)
    ()
  }

}

object LogKind extends Enumeration {
  val Debug, Info, Warning, Error = Value
}

case class LogEntry(time: LocalDateTime, kind: LogKind.Value, message: String, exOpt: Option[Throwable] = None)
