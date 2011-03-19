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

import org.scalatest.{Spec, BeforeAndAfterEach}
import org.scalatest.matchers.MustMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import net.lag.configgy.Configgy
import ConfiggyBackend._

@RunWith(classOf[JUnitRunner])
class configgySpec extends Spec with MustMatchers with BeforeAndAfterEach {
  override def beforeEach {
    super.beforeEach
    ConfigFactory.clear()
    ConfigFactory.add( Configgy.config )
  }

  override def afterEach {
    ConfigFactory.clear()
    super.afterEach
  }

  describe("ConfiggyBackend") {
    Configgy.configureFromResource("test-configgy.conf")

    it("should load by class name") {
      ConfigFactory.get[TestConfig](classOf[TestConfig].getName) must equal(TestConfig("debug", List("aaa", "bbb")))
    }
    it("should load nested config") {
      case class Nested(val k: Int, val s: String, val b1: Boolean, val b2: Boolean)
      case class Test2(val i: Int, val nested: Nested)

      ConfigFactory.get[Test2]("test2") must equal(Test2(103, Nested(10, "ab", true, false)))
    }
    it("should load empty array") {
      case class EmptyTest(val b: Boolean, val ary: List[String])

      ConfigFactory.get[EmptyTest]("empty") must equal(EmptyTest(false, Nil))
    }
  }
}
