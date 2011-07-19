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
package backend

/**
 * @author Alexander Azarov <azarov@osinka.ru>
 */
class MockBackend(val configs: Map[String,AnyRef]) extends ConfigBackend {
  def get[T : Manifest](name: String): Option[T] =
    configs.get(name) filter {implicitly[Manifest[T]].erasure.isInstance} map { _.asInstanceOf[T] }

  override def toString = "MockBackend [%s]" format configs.keySet.mkString(", ")
}

/**
 * @author Alexander Azarov <azarov@osinka.ru>
 */
object MockBackend {
  def apply(configs: Map[String,AnyRef]) = new MockBackend(configs)
  def apply(configs: (String,AnyRef)*): MockBackend = apply(Map(configs:_*))
}
