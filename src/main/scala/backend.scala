package com.osinka.confback

trait ConfigBackend {
  def get[T : Manifest](name: String): Option[T]
  def subscribe(f: => Unit): Unit
}
