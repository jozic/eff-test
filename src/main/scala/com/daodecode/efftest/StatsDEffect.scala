package com.daodecode.efftest

import cats.data.Writer
import org.atnos.eff._, all._, syntax.all._
import StatsD._


trait StatsDTypes {
  type StatsdWriter[A] = Writer[Metric, A]
  type _statsd[R] = StatsdWriter |= R

  type _StatsD[R] = StatsdWriter <= R
}

trait StatsDCreation extends StatsDTypes {

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

trait StatsDInterpretation extends StatsDTypes {

  implicit class StatsDEffectOps[R, A](e: Eff[R, A]) {
    def runStatsDUnsafe(statsD: StatsD)(implicit member: _StatsD[R]): Eff[member.Out, A] =
      e.runWriterUnsafe[Metric](statsD.send)

    def runStatsD(implicit member: _StatsD[R]): Eff[member.Out, (A, List[Metric])] =
      e.runWriter[Metric]

    def runStatsDIgnoreMetrics(implicit member: _StatsD[R]): Eff[member.Out, A] =
      e.runWriterNoLog[Metric]
  }

}

trait StatsDEffect extends StatsDCreation with StatsDInterpretation

object StatsDEffect extends StatsDEffect
