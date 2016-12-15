package com.daodecode.efftest

import cats.data.Writer
import org.atnos.eff._, all._
import org.atnos.eff.syntax.safe._
import StatsD._


trait EffStatsD {

  type StatsdWriter[A] = Writer[Metric, A]
  type _statsd[R] = StatsdWriter |= R

  def counter[R: _statsd](label: String, count: Long = 1): Eff[R, Unit] = for {
    _ <- tell[R, Metric](Counter(label, count))
  } yield ()

  def timing[R: _statsd](label: String, time: Long): Eff[R, Unit] = for {
    _ <- tell[R, Metric](Timing(label, time))
  } yield ()

  // timing is not correct
  def withTiming[R: _statsd : _Safe, A](label: String)(eff: Eff[R, A]): Eff[R, A] =
    for {
      start <- protect(System.currentTimeMillis())
      result <- eff.thenFinally {
        timing(label, System.currentTimeMillis() - start)
      }
    } yield result
}
