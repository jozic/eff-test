package com.daodecode.efftest

import cats.data.Writer
import org.atnos.eff._, all._, syntax.writer
import Logger._

trait LoggerTypes {
  type LogWriter[A] = Writer[LogEntry, A]
  type _log[R] = LogWriter |= R
  type _Log[R] = LogWriter <= R

}

trait LoggerCreation extends LoggerTypes {
  private def log[R: _log](logEntry: LogEntry): Eff[R, Unit] = tell(logEntry)

  def debug[R: _log](s: => String): Eff[R, Unit] = log(new Debug(s))

  def info[R: _log](s: => String): Eff[R, Unit] = log(new Info(s))

  def warn[R: _log](s: => String): Eff[R, Unit] = log(new Warn(s))

  def error[R: _log](s: => String): Eff[R, Unit] = log(new Error(s))

  def error[R: _log](s: => String, t: Throwable): Eff[R, Unit] = log(new Error(s, Some(t)))

}

trait LoggerInterpretation extends LoggerTypes with writer {
  implicit class LoggerEffectOps[R, A](e: Eff[R, A]) {
    def runLoggerUnsafe(logger: Logger)(implicit member: _Log[R]): Eff[member.Out, A] =
      e.runWriterUnsafe[LogEntry](logger.log)

    def runLogger(implicit member: _Log[R]): Eff[member.Out, (A, List[LogEntry])] =
      e.runWriter[LogEntry]

    def runLoggerIgnoreLogs(implicit member: _Log[R]): Eff[member.Out, A] =
      e.runWriterNoLog[LogEntry]
  }

}

trait LoggerEffect extends LoggerCreation with LoggerInterpretation

object LoggerEffect extends LoggerEffect
