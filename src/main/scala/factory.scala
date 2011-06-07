/**
 * Copyright (C) 2011 Alexander Azarov <azarov@osinka.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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

  /**
   * To mock configuration
   */
  def runWith[R](mock: ConfigBackend*)(f: => R): R =
    ultimately {
      backends = backends.takeRight(backends.length - mock.length)
    } apply {
      backends = mock.toList ::: backends
      f
    }

  def key[T](implicit m: Manifest[T]) = m.erasure.getName

  def refreshOrCached[T : Manifest]: Option[T] = refreshOrCached[T](key[T])

  def refreshOrCached[T : Manifest](name: String): Option[T] =
    reget[T](name) orElse opt[T](name)

  def get[T : Manifest]: T = get[T](key[T])

  def opt[T : Manifest]: Option[T] = opt[T](key[T])

  def get[T : Manifest](name: String): T = opt[T](name) getOrElse { throw new ConfigurationNotFoundException(name) }

  def reget[T : Manifest](name: String): Option[T] =
    lookupConfig[T](name) map { t => 
      configs.update(name, t)
      t._2
    }

  def opt[T : Manifest](name: String): Option[T] =
    synchronized {
      configs.get(name) map { _._2.asInstanceOf[T] } orElse { reget[T](name) }
    }

  def subscribe(key: String, f: => Unit) {
    watchers(key) = {() => f} :: watchers.getOrElse(key, Nil)
  }

  def unsubscribe(key: String) {
    watchers.remove(key)
  }

  protected def lookupConfig[T : Manifest](key: String): Option[(ConfigBackend, T)] = {
    assert(backends.nonEmpty)

    val names = {
      val (pkg, clz) = {
        val s = key split "\\."
        (s.init.toList, s.last)
      }

      val packageVariants = pkg.scanRight(List.empty[String]) { _ :: _ }.tail.reverse
      val clzVariants = {
        def firstLower(s: String) = s.substring(0,1).toLowerCase + s.substring(1)
        def withLowerCase(s: String) = s :: firstLower(s) :: Nil
        
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
     } ).headOption
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
