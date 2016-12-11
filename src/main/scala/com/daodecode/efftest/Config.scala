package com.daodecode.efftest

trait Config {

  def string(key: String): String

  def long(key: String): Long
}

case class MapConfig(kvs: (String, Any)*) extends Config {
  private val m = kvs.toMap

  override def string(key: String): String = m(key).asInstanceOf[String]

  override def long(key: String): Long = m(key).asInstanceOf[Long]
}
