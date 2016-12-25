package com.daodecode.efftest

import cats.data.Writer
import org.atnos.eff._, all._
import org.atnos.eff.syntax.safe._
import StatsD._


trait EffStatsD {

  type StatsdWriter[A] = Writer[Metric, A]
  type _statsd[R] = StatsdWriter |= R

  private def send[R: _statsd](metric: Metric): Eff[R, Unit] = tell(metric)

  def counter[R: _statsd](label: String, count: Long = 1): Eff[R, Unit] = send(Counter(label, count))

  def timing[R: _statsd](label: String, time: => Long): Eff[R, Unit] = send(new Timing(label, time))

  def withTiming[R: _statsd : _Safe, A](label: String)(eff: Eff[R, A]): Eff[R, A] =
    for {
      start <- protect(System.currentTimeMillis())
      result <- eff.thenFinally {
        timing(label, System.currentTimeMillis() - start)
      }
    } yield result
}
