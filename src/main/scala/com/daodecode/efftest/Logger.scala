package com.daodecode.efftest

trait Logger {

  def info(m: => String): Unit

  def debug(m: => String): Unit

  def error(m: => String, t: Throwable): Unit

}

object ConsoleLogger extends Logger {
  override def info(m: => String): Unit = {
    println(s"INFO: $m")
  }

  override def debug(m: => String): Unit = {
    println(s"DEBUG: $m")
  }

  override def error(m: => String, t: Throwable): Unit = {
    println(s"ERROR: $m caused by ${t.printStackTrace()}")
  }
}


