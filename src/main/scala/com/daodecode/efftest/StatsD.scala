package com.daodecode.efftest

import com.daodecode.efftest.StatsD.{Counter, Timing}

trait StatsD {

  import StatsD._

  def increment(c: Counter): Unit

  def increment(label: String, counter: Long = 1): Unit = increment(Counter(label, counter))

  def timing(t: => Timing): Unit

  def timing(label: String, t: => Long): Unit = timing(new Timing(label, t))

  def send(m: Metric): Unit = m match {
    case c: Counter => increment(c)
    case t: Timing => timing(t)
  }

}

object StatsD {

  sealed trait Metric

  case class Counter(label: String, count: Long = 1) extends Metric

  class Timing(val label: String, t: => Long) extends Metric {
    lazy val time = t
  }

}

object ConsoleStatsD extends StatsD {

  override def increment(c: Counter): Unit = {
    println(s"STATSD: incrementing [${c.label}] by ${c.count}")
  }

  override def timing(t: => Timing): Unit = {
    println(s"STATSD: recording timing of [${t.label}]: ${t.time}")
  }
}
