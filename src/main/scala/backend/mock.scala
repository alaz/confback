package com.osinka.confback
package backend

class MockBackend(val configs: Map[String,Any]) extends ConfigBackend {
  def get[T : Manifest](name: String): Option[T] =
    configs.get(name) filter { implicitly[Manifest[T]].erasure.isInstance } map { _.asInstanceOf[T] }

  override def subscribe(f: => Unit) {}
}

object MockBackend {
  def apply(configs: Map[String,Any]) = new MockBackend(configs)
  def apply(configs: (String,Any)*): MockBackend = apply(Map(configs:_*))
}
