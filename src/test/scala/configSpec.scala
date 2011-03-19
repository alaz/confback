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
