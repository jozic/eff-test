package com.daodecode.efftest

abstract class Program(val name: String) {
  def compute(): Long

  // some computation that takes some time and can throw an error
  protected def someCompute(a: Long, b: Long): Long = {
    Thread.sleep(100)
    (util.Random.nextLong() + a / b) * b
  }

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

class Program2(config: Config, logger: Logger, statsD: StatsD)
  extends Program("effs-again") with EffLogger {

  import cats._, data._
  import org.atnos.eff._, all._
  import org.atnos.eff.syntax.all._
  import StatsDEffect._

  type ConfigReader[A] = Reader[Config, A]

  type _config[R] = ConfigReader |= R

  type Stack = Fx.fx4[ConfigReader, LogWriter, StatsdWriter, Safe]

  def computer[R: _config : _log : _statsd : _Safe]: Eff[R, Long] =
    for {
      cfg <- ask
      _ <- debug("reading a and b")
      a <- protect(cfg.long("a"))
      b <- protect(cfg.long("b"))
      _ <- info(s"a is [$a], b is [$b]")
      label <- pure(cfg.string("statsd.label"))
      _ <- counter(s"$label.a", a)
      _ <- counter(s"$label.b", b)
      result <- protect(someCompute(a, b))
      _ <- counter(s"$label.result", result)
    } yield result

  def withTimer = withTiming("compute.time")(computer[Stack])

  def compute(): Long = {
    withTimer
      .runReader(config)
      .runWriterUnsafe[Logger.LogEntry](logger.log)
      .runStatsDUnsafe(statsD)
      .execSafe
      .run.toTry.get
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
//    val cfg = MapConfig("a" -> 1231L, "b1" -> 0L, "statsd.label" -> "boo")

  runProgram(new Program1(
    config = cfg,
    logger = ConsoleLogger,
    statsD = ConsoleStatsD
  ))

  runProgram(new Program2(
    config = cfg,
    logger = ConsoleLogger,
    statsD = ConsoleStatsD
  ))

}
