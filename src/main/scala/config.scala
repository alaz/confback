package com.osinka.confback

/**
 * Configuration case class should extend this trait
 */
trait Configuration

/**
 * Configurable entity extends this trait
 */
trait Configurable[T <: Configuration] {
  def configKey(implicit m: Manifest[T]) = m.erasure.getName

  def whenChanged: List[() => Unit] = Nil

  def config(implicit m: Manifest[T]): T = ConfigService.get[T](configKey)

  def subscribeConfigChanges(implicit m: Manifest[T]) {
    ConfigService.subscribe(configKey,  () => whenChanged foreach { _() } )
  }

  def unsubscribeConfigChanges(implicit m: Manifest[T]) {
    ConfigService.unsubscribe(configKey)
  }
}
