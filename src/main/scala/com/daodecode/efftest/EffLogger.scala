package com.daodecode.efftest

import cats.data.Writer
import org.atnos.eff._, all._
import Logger._

trait EffLogger {

  type LogWriter[A] = Writer[LogEntry, A]
  type _log[R] = LogWriter |= R

  def info[R: _log](s: => String): Eff[R, Unit] = for {
    _ <- tell[R, LogEntry](Info(s))
  } yield ()

  def debug[R: _log](s: => String): Eff[R, Unit] = for {
    _ <- tell[R, LogEntry](Debug(s))
  } yield ()

  def error[R: _log](s: => String): Eff[R, Unit] = for {
    _ <- tell[R, LogEntry](Error(s))
  } yield ()

  def error[R: _log](s: => String, t: Throwable): Eff[R, Unit] = for {
    _ <- tell[R, LogEntry](Error(s, Some(t)))
  } yield ()

}
