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
