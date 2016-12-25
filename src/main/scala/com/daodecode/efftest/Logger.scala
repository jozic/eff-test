package com.daodecode.efftest

trait Logger {

  import Logger._

  def info(m: => String): Unit

  def debug(m: => String): Unit

  def warn(m: => String): Unit

  def error(m: => String, to: => Option[Throwable] = None): Unit

  def log(entry: LogEntry): Unit = entry match {
    case _: Debug => debug(entry.message)
    case _: Info => info(entry.message)
    case _: Warn => warn(entry.message)
    case _: Error => error(entry.message, entry.ex)
  }

}

object Logger {

  sealed abstract class LogEntry(m: => String, tho: => Option[Throwable] = None) {
    lazy val message: String = m
    lazy val ex: Option[Throwable] = tho
  }

  class Debug(msg: => String) extends LogEntry(msg)

  class Info(msg: => String) extends LogEntry(msg)

  class Warn(msg: => String) extends LogEntry(msg)

  class Error(msg: => String, tho: => Option[Throwable] = None) extends LogEntry(msg, tho)

}

object ConsoleLogger extends Logger {
  override def debug(m: => String): Unit = {
    println(s"DEBUG: $m")
  }

  override def info(m: => String): Unit = {
    println(s"INFO: $m")
  }

  override def warn(m: => String): Unit = {
    println(s"WARN: $m")
  }

  override def error(m: => String, to: => Option[Throwable] = None): Unit = {
    println(s"ERROR: $m${to.map(t => s"caused by ${t.getMessage}").getOrElse("")}")
  }
}
