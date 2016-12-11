package com.daodecode.efftest

import math._

abstract class Program(val name: String) {
  def compute(): Long

  // some computation that takes some time and can throw an error
  protected def someCompute(a: Long, b: Long): Long =
    (0 to abs(max(abs(a), abs(b)).toInt)).map { i =>
      (util.Random.nextLong() + a / b) * b + i
    }.max

}

class Program1(config: Config, logger: Logger, statsD: StatsD) extends Program("simple") {

  override def compute(): Long = {
    val start = System.currentTimeMillis()
    try {
      logger.debug("reading a and b")
      val a = config.long("a")
      val b = config.long("b")
      logger.info(s"a is [$a], b is [$b]")
      val label = config.string("statsd.label")
      statsD.increment(s"$label.a", a)
      statsD.increment(s"$label.b", b)
      val result = someCompute(a, b)
      statsD.increment(s"$label.result", result)
      result
    } finally {
      statsD.timing("compute.time", System.currentTimeMillis() - start)
    }
  }

}

class Program2(config: Config, logger: Logger, statsD: StatsD) extends Program("first-effs") {

  import cats._, data._
  import org.atnos.eff._, all._
  import org.atnos.eff.syntax.all._

  type ConfigReader[A] = Reader[Config, A]
  type LogWriter[A] = Writer[String, A]
  type StatsdCounterWriter[A] = Writer[StatsD.Counter, A]
  type StatsdTimingWriter[A] = Writer[StatsD.Timing, A]

  type _config[R] = ConfigReader |= R
  type _log[R] = LogWriter |= R
  type _statsdCounter[R] = StatsdCounterWriter |= R
  type _statsdTiming[R] = StatsdTimingWriter |= R

  type Stack = Fx.fx5[ConfigReader, LogWriter, StatsdCounterWriter, StatsdTimingWriter, Eval]

  def computer[R: _config : _log : _statsdCounter : _eval]: Eff[R, Long] =
    for {
      _ <- tell("reading a and b")
      cfg <- ask
      a <- pure(cfg.long("a"))
      b <- pure(cfg.long("b"))
      _ <- tell(s"a is [$a], b is [$b]")
      label <- pure(cfg.string("statsd.label"))
      _ <- tell(StatsD.Counter(s"$label.a", a))
      _ <- tell(StatsD.Counter(s"$label.b", b))
      result <- delay(someCompute(a, b))
      _ <- tell(StatsD.Counter(s"$label.result", result))
    } yield result

  // not safe, use Safe/andFinally
  def withTimer[R: _config : _log : _statsdCounter : _statsdTiming : _eval]: Eff[R, Long] =
    for {
      start <- pure(System.currentTimeMillis())
      result <- computer
      _ <- tell(StatsD.Timing("compute.time", System.currentTimeMillis() - start))
    } yield result

  override def compute(): Long = {
    withTimer[Stack]
      .runReader(config)
      .runWriterUnsafe[String](logger.debug(_))
      .runWriterUnsafe[StatsD.Counter](statsD.increment)
      .runWriterUnsafe[StatsD.Timing](statsD.timing)
      .runEval
      .run
  }

}

object ProgramApp extends App {

  def runProgram(p: Program): Unit = {
    println(s"===========RUNNING PROGRAM [${p.name}] ===========")
    try {
      val result = p.compute()
      println(s"Final result it [$result]")
    } catch {
      case t: Throwable =>
        println(s"GOT EXCEPTION: ${t.getMessage}")
    }
    println(s"===========PROGRAM [${p.name}] FINISHED===========")
  }

  val cfg = MapConfig("a" -> 1231L, "b" -> -23412L, "statsd.label" -> "boo")
  //  val cfg = MapConfig("a" -> 1231L, "b" -> 0L, "statsd.label" -> "boo")

  runProgram(new Program2(
    config = cfg,
    logger = ConsoleLogger,
    statsD = ConsoleStatsD
  ))

  runProgram(new Program1(
    config = cfg,
    logger = ConsoleLogger,
    statsD = ConsoleStatsD
  ))

}
