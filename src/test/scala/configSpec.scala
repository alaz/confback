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
import org.scalatest.fixture.FixtureSpec
import org.scalatest.matchers.MustMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class configSpec extends FixtureSpec with MustMatchers {
  type FixtureParam = MockBackend

  override def withFixture(test: OneArgTest) {
    ultimately {
      ConfigFactory.clear()
    } apply {
      val backend = new MockBackend("test")
      ConfigFactory.clear()
      ConfigFactory.add(backend)
      test(backend)
    }
  }

  describe("Config key discovery") {
    it("should cache config") { backend =>
      ConfigFactory.get[Config]("test") must equal(Config("test", 1))
      val n = backend.subscriptions

      ConfigFactory.get[Config]("test") must equal(Config("test", 1))
      backend.subscriptions must equal(n)
    }
    it("should discover by exact name") { backend =>
      ConfigFactory.get[Config]("test") must equal(Config("test", 1))
      backend.subscriptions must equal(1)
      backend.namesAsked must equal(List("test"))
    }
    it("should discover by capitalized name") { backend =>
      ConfigFactory.get[Config]("Test") must equal(Config("test", 1))
    }
    it("should discover when Config has suffix") { backend =>
      ConfigFactory.get[Config]("TestConfig") must equal(Config("test", 1))
    }
    it("should discover when package name given") { backend =>
      ConfigFactory.get[Config]("com.osinka.config.TestConfig") must equal(Config("test", 1))
    }
    it("should find package name as priority") { backend =>
      ConfigFactory.add(new MockBackend("config.test"))
      ConfigFactory.get[Config]("com.osinka.config.TestConfig") must equal(Config("config.test", 1))
    }
  }

  case class Config(val name: String, val serial: Int)

  class MockBackend(val positiveName: String) extends ConfigBackend {
    var subscriptions = 0
    var serial = 0
    val namesAsked = collection.mutable.ListBuffer[String]()

    override def get[T : Manifest](name: String): Option[T] = {
      namesAsked += name

      if (implicitly[Manifest[T]].erasure == classOf[Config] && name == positiveName) {
        serial = serial + 1
        Some( new Config(name, serial).asInstanceOf[T] )
      } else
        None
    }

    override def subscribe(f: => Unit) {
      subscriptions += 1
    }
  }
}
