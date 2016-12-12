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

  import StatsD._
  import Logger._

  type ConfigReader[A] = Reader[Config, A]
  type LogWriter[A] = Writer[LogEntry, A]
  type StatsdWriter[A] = Writer[Metric, A]

  type _config[R] = ConfigReader |= R
  type _log[R] = LogWriter |= R
  type _statsd[R] = StatsdWriter |= R

  type Stack = Fx.fx4[ConfigReader, LogWriter, StatsdWriter, Eval]

  def computer[R: _config : _log : _statsd : _eval]: Eff[R, Long] =
    for {
      cfg <- ask
      _ <- tell(debug("reading a and b"))
      a <- pure(cfg.long("a"))
      b <- pure(cfg.long("b"))
      _ <- tell(info(s"a is [$a], b is [$b]"))
      label <- pure(cfg.string("statsd.label"))
      _ <- tell(counter(s"$label.a", a))
      _ <- tell(counter(s"$label.b", b))
      result <- delay(someCompute(a, b))
      _ <- tell(counter(s"$label.result", result))
    } yield result

  // not safe, use Safe/andFinally
  def withTimer[R: _config : _log : _statsd : _eval]: Eff[R, Long] =
    for {
      start <- pure(System.currentTimeMillis())
      result <- computer
      _ <- tell(timing("compute.time", System.currentTimeMillis() - start))
    } yield result

  override def compute(): Long = {
    withTimer[Stack]
      .runReader(config)
      .runWriterUnsafe[LogEntry](logger.log)
      .runWriterUnsafe[Metric](statsD.send)
      .runEval
      .run
  }

}

trait EffLogger {

  import cats.data.Writer
  import org.atnos.eff._, all._
  import Logger._

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

trait EffStatsD {

  import cats.data.Writer
  import org.atnos.eff._, all._

  import StatsD._

  type StatsdWriter[A] = Writer[Metric, A]
  type _statsd[R] = StatsdWriter |= R

  def counter[R: _statsd](label: String, count: Long = 1): Eff[R, Unit] = for {
    _ <- tell[R, Metric](Counter(label, count))
  } yield ()

  def timing[R: _statsd](label: String, time: Long): Eff[R, Unit] = for {
    _ <- tell[R, Metric](Timing(label, time))
  } yield ()

}

class Program3(config: Config, logger: Logger, statsD: StatsD)
  extends Program("effs-again") with EffLogger with EffStatsD {

  import cats._, data._
  import org.atnos.eff._, all._
  import org.atnos.eff.syntax.all._

  type ConfigReader[A] = Reader[Config, A]

  type _config[R] = ConfigReader |= R

  type Stack = Fx.fx4[ConfigReader, LogWriter, StatsdWriter, Eval]

  def computer[R: _config : _log : _statsd : _eval]: Eff[R, Long] =
    for {
      cfg <- ask
      _ <- debug("reading a and b")
      a <- pure(cfg.long("a"))
      b <- pure(cfg.long("b"))
      _ <- info(s"a is [$a], b is [$b]")
      label <- pure(cfg.string("statsd.label"))
      _ <- counter(s"$label.a", a)
      _ <- counter(s"$label.b", b)
      result <- delay(someCompute(a, b))
      _ <- counter(s"$label.result", result)
    } yield result

  // not safe, use Safe/andFinally
  def withTimer[R: _config : _log : _statsd : _eval]: Eff[R, Long] =
    for {
      start <- pure(System.currentTimeMillis())
      result <- computer
      _ <- timing("compute.time", System.currentTimeMillis() - start)
    } yield result

  override def compute(): Long = {
    withTimer[Stack]
      .runReader(config)
      .runWriterUnsafe[Logger.LogEntry](logger.log)
      .runWriterUnsafe[StatsD.Metric](statsD.send)
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

  runProgram(new Program3(
    config = cfg,
    logger = ConsoleLogger,
    statsD = ConsoleStatsD
  ))

}
