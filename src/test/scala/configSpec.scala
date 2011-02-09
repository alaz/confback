package com.osinka.confback

import org.scalatest.{Spec, BeforeAndAfterEach}
import org.scalatest.matchers.MustMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import net.lag.configgy.Configgy
import backend._

case class TestConfig(val level: String, val ary: List[String]) extends Configuration

@RunWith(classOf[JUnitRunner])
class configSpec extends Spec with MustMatchers with BeforeAndAfterEach {
  override def beforeEach {
    super.beforeEach
    ConfigService.clear()
  }

  override def afterEach {
    ConfigService.clear()
    super.afterEach
  }

  describe("ConfiggyBackend") {
    Configgy.configureFromResource("test-configgy.conf")

    it("should load by class name") {
      ConfigService.add(new ConfiggyBackend(Configgy.config))
      ConfigService.get[TestConfig](classOf[TestConfig].getName) must equal(TestConfig("debug", List("aaa", "bbb")))
    }
    it("should load nested config") {
      case class Nested(val k: Int, val s: String, val b1: Boolean, val b2: Boolean)
      case class Test2(val i: Int, val nested: Nested) extends Configuration

      ConfigService.add(new ConfiggyBackend(Configgy.config))
      ConfigService.get[Test2]("test2") must equal(Test2(103, Nested(10, "ab", true, false)))
    }
    it("should load empty array") {
      case class EmptyTest(val b: Boolean, val ary: List[String]) extends Configuration

      ConfigService.add(new ConfiggyBackend(Configgy.config))
      ConfigService.get[EmptyTest]("empty") must equal(EmptyTest(false, Nil))
    }
  }
  describe("Config key discovery") {
    it("should cache config") {
      val backend = new MockBackend("test")
      ConfigService.add(backend)
      
      ConfigService.get[Config]("test") must equal(Config("test", 1))
      val n = backend.subscriptions

      ConfigService.get[Config]("test") must equal(Config("test", 1))
      backend.subscriptions must equal(n)
    }
    it("should discover by exact name") {
      val backend = new MockBackend("test")
      ConfigService.add(backend)

      ConfigService.get[Config]("test") must equal(Config("test", 1))
      backend.subscriptions must equal(1)
      backend.namesAsked must equal(List("test"))
    }
    it("should discover by capitalized name") {
      ConfigService.add(new MockBackend("test"))
      ConfigService.get[Config]("Test") must equal(Config("test", 1))
    }
    it("should discover when Config has suffix") {
      ConfigService.add(new MockBackend("test"))
      ConfigService.get[Config]("TestConfig") must equal(Config("test", 1))
    }
    it("should discover when package name given") {
      ConfigService.add(new MockBackend("test"))
      ConfigService.get[Config]("com.osinka.config.TestConfig") must equal(Config("test", 1))
    }
    it("should find package name as priority") {
      ConfigService.add(new MockBackend("config.test"))
      ConfigService.add(new MockBackend("test"))
      ConfigService.get[Config]("com.osinka.config.TestConfig") must equal(Config("config.test", 1))
    }
  }

  case class Config(val name: String, val serial: Int) extends Configuration

  class MockBackend(val positiveName: String) extends ConfigBackend {
    var subscriptions = 0
    var serial = 0
    val namesAsked = collection.mutable.ListBuffer[String]()

    override def get[T <: Configuration : Manifest](name: String): Option[T] = {
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
