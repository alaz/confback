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

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class mockSpec extends Spec with MustMatchers {
  case class Config(val name: String)

  override def withFixture(test: NoArgTest) {
    ConfigFactory.runWith(MockBackend("testMe" -> Config("value"))) { test() }
  }

  describe("MockBackend") {
    it("should return config by long key") {
      ConfigFactory.get[Config]("config.testMe") must equal(Config("value"))
    }
    it("should return config by short name") {
      ConfigFactory.get[Config]("TestMe") must equal(Config("value"))
      ConfigFactory.get[Config]("testMe") must equal(Config("value"))
    }
    it("should not return by another key") {
      ConfigFactory.opt[Config]("test2") must equal(None)
      ConfigFactory.opt[Config]("TESTME") must equal(None)
    }
    it("should provide subclasses") {
      val backend = MockBackend("test" -> new Config("value") { def f {} })
      backend.get[Config]("test") must be('defined)
    }
  }
}
