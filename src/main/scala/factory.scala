package com.osinka.confback

import util.control.Exception._
import org.slf4j.LoggerFactory

class ConfigurationNotFoundException(val key: String) extends Throwable("Configuration key "+key+" not found or failed to load")

object ConfigFactory {
  private val logger = LoggerFactory.getLogger(getClass.getName)

  import collection.mutable.Map

  private var backends: List[ConfigBackend] = Nil
  private val configs = Map.empty[String, (ConfigBackend, Any)]
  private val watchers = Map.empty[String, List[() => Unit]]

  // for testing
  private[confback] def clear() {
    backends = Nil
    configs.clear()
    watchers.clear()
  }

  def add(backend: ConfigBackend) {
    logger.debug("Added configuration backend %s".format(backend))
    backends = backend :: backends
  }

  def key[T](implicit m: Manifest[T]) = m.erasure.getName

  def get[T : Manifest]: T = get[T](key[T])

  def opt[T : Manifest]: Option[T] = opt[T](key[T])

  def get[T : Manifest](name: String): T =
    synchronized {
      val (backend, config) = configs.getOrElseUpdate(name, lookupConfig(name))
      config.asInstanceOf[T]
    }

  def opt[T : Manifest](name: String): Option[T] =
    handling(classOf[Throwable]) by {t =>
      logger.warn("Failed to load configuration by key %s".format(name))
      None
    } apply { Some(get[T](name)) }

  def subscribe(key: String, f: => Unit) {
    watchers(key) = {() => f} :: watchers.getOrElse(key, Nil)
  }

  def unsubscribe(key: String) {
    watchers.remove(key)
  }

  protected def lookupConfig[T : Manifest](key: String): (ConfigBackend, T) = {
    assert(backends.nonEmpty)

    val names = {
      val (pkg, clz) = {
        val s = key split "\\."
        (s.init.toList, s.last)
      }

      val packageVariants = pkg.scanRight(List.empty[String]) { _ :: _ }.tail.reverse
      val clzVariants = {
        def withLowerCase(s: String) = s :: s.toLowerCase :: Nil
        
        (withLowerCase(clz) ::: withLowerCase(clz stripSuffix "Config")).distinct
      }

      (packageVariants.flatMap{p => clzVariants map {c => p ::: c :: Nil} }.map{ _ mkString "." } ::: clzVariants).distinct
    }

    logger.trace("Config names to try: %s".format(names.mkString(", ")))

    (for {n <- names.toStream
          backend <- backends
          config <- backend.get[T](n)}
     yield {
       logger.debug("Found configuration %s for `%s` backend %s".format(n, key, backend))
       backend subscribe {
         logger.info("Configuration %s has changed".format(n))
         watchers(key) foreach {_()}
       }

       (backend, config)
     } ).headOption.getOrElse(throw new ConfigurationNotFoundException(key))
  }

  // init
  handling(classOf[Throwable]) by { t =>
    logger.error("Failed to load class bootstrap.confback.Boot", t)
  } apply {
    Class.forName("bootstrap.confback.Boot").newInstance
    if (backends.isEmpty)
      logger.error("No configuration backends. You have to add them in bootstrap.confback.Boot!")
  }
}
