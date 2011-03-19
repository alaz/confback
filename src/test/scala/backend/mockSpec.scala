package com.osinka.confback
package backend

import org.scalatest.{Spec, BeforeAndAfterEach}
import org.scalatest.matchers.MustMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class mockSpec extends Spec with MustMatchers with BeforeAndAfterEach {
  case class Config(val name: String)

  override def beforeEach {
    super.beforeEach
    ConfigFactory.clear()
    ConfigFactory.add( MockBackend("test" -> Config("value")) )
  }

  override def afterEach {
    ConfigFactory.clear()
    super.afterEach
  }

  describe("MockBackend") {
    it("should return config by long key") {
      ConfigFactory.get[Config]("config.test") must equal(Config("value"))
    }
    it("should return config by short name") {
      ConfigFactory.get[Config]("TEST") must equal(Config("value"))
    }
    it("should not return by another key") {
      ConfigFactory.opt[Config]("test2") must equal(None)
    }
  }
}
