package com.daodecode.efftest

import cats.data.Writer
import org.atnos.eff._, all._
import Logger._

trait EffLogger {

  type LogWriter[A] = Writer[LogEntry, A]
  type _log[R] = LogWriter |= R

  def debug[R: _log](s: => String): Eff[R, Unit] = log(new Debug(s))

  def info[R: _log](s: => String): Eff[R, Unit] = log(new Info(s))

  def warn[R: _log](s: => String): Eff[R, Unit] = log(new Warn(s))

  def error[R: _log](s: => String): Eff[R, Unit] = log(new Error(s))

  def error[R: _log](s: => String, t: Throwable): Eff[R, Unit] = log(new Error(s, Some(t)))

  private def log[R: _log](logEntry: LogEntry): Eff[R, Unit] = for {
    _ <- tell(logEntry)
  } yield ()

}
