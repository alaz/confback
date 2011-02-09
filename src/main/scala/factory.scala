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

  def get[T : Manifest](name: String): T =
    synchronized {
      val (backend, config) = configs.getOrElseUpdate(name, lookupConfig(name))
      config.asInstanceOf[T]
    }

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
  }
}

// Convenience implicits for Boot
trait ConfigBootstrap {
  import backend._
  import net.lag.configgy.ConfigMap

  implicit def fromConfiggy(configMap: ConfigMap) = new ConfiggyBackend(configMap)
}
