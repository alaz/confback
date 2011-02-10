package com.osinka.confback

/**
 * Configurable entity extends this trait
 */
trait Configurable[T] {
  def configKey(implicit m: Manifest[T]) = m.erasure.getName

  def whenChanged: List[() => Unit] = Nil

  def configOpt(implicit m: Manifest[T]): Option[T] = ConfigFactory.opt[T](configKey)

  def config(implicit m: Manifest[T]): T = ConfigFactory.get[T](configKey)

  def subscribeConfigChanges(implicit m: Manifest[T]) {
    ConfigFactory.subscribe(configKey,  () => whenChanged foreach { _() } )
  }

  def unsubscribeConfigChanges(implicit m: Manifest[T]) {
    ConfigFactory.unsubscribe(configKey)
  }
}
