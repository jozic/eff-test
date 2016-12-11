package com.daodecode.efftest

import com.daodecode.efftest.StatsD.{Counter, Timing}

trait StatsD {

  import StatsD._

  def increment(c: Counter): Unit

  def increment(label: String, counter: Long = 1): Unit = increment(Counter(label, counter))

  def timing(t: Timing): Unit

  def timing(label: String, time: Long = 1): Unit = timing(Timing(label, time))

}

object StatsD {

  case class Counter(label: String, count: Long = 1)

  case class Timing(label: String, time: Long)

}

object ConsoleStatsD extends StatsD {

  override def increment(c: Counter): Unit = {
    println(s"STATSD: incrementing [${c.label}] by ${c.count}")
  }

  override def timing(t: Timing): Unit = {
    println(s"STATSD: recording timing of [${t.label}]: ${t.time}")
  }
}
