package com.daodecode.efftest

trait Logger {

  import Logger._

  def info(m: => String): Unit

  def debug(m: => String): Unit

  def error(m: => String, to: Option[Throwable] = None): Unit

  def log(level: Level, m: => String, to: Option[Throwable] = None): Unit = level match {
    case InfoLevel => info(m)
    case DebugLevel => debug(m)
    case ErrorLevel => error(m, to)
  }

  def log(entry: LogEntry): Unit = log(entry.level, entry.m, entry.to)

}

object Logger {

  sealed trait Level

  case object InfoLevel extends Level

  case object DebugLevel extends Level

  case object ErrorLevel extends Level

  sealed abstract class LogEntry(val level: Level, val m: String, val to: Option[Throwable] = None)

  case class Info(msg: String) extends LogEntry(InfoLevel, msg)

  case class Debug(msg: String) extends LogEntry(DebugLevel, msg)

  case class Error(msg: String, tho: Option[Throwable] = None) extends LogEntry(ErrorLevel, msg, tho)

  def info(msg: String): LogEntry = Info(msg)

  def debug(msg: String): LogEntry = Debug(msg)

  def error(msg: String, to: Option[Throwable] = None): LogEntry = Error(msg, to)

}

object ConsoleLogger extends Logger {
  override def info(m: => String): Unit = {
    println(s"INFO: $m")
  }

  override def debug(m: => String): Unit = {
    println(s"DEBUG: $m")
  }

  override def error(m: => String, to: Option[Throwable] = None): Unit = {
    println(s"ERROR: $m${to.map(t => s"caused by ${t.getMessage}").getOrElse("")}")
  }
}


