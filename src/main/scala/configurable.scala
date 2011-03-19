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
