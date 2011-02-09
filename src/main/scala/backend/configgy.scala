package com.osinka.confback
package backend

import util.control.Exception._
import net.lag.configgy.{Configgy, ConfigMap}
import net.liftweb.json.{JsonAST, JsonDSL, Extraction}
import org.slf4j.LoggerFactory

class ConfiggyBackend(val configMap: ConfigMap = Configgy.config) extends ConfigBackend {
  private val logger = LoggerFactory.getLogger(getClass.getName)

  import JsonAST._

  val ArrayRE = """^\[(.*)\]$""".r

  def json(cm: ConfigMap): JObject = {
    import net.lag.configgy.ConfigException
    import JsonDSL._

    def detectString(key: String)(s: String) =
      s match {
        case ArrayRE(_) => None
        case _ => Some(s)
      }

    def detectList(key: String)(l: Seq[String]) =
      if (l.isEmpty) None // TODO: or should we return JField(key, JNothing)
      else Some(JField(key, l.toList))

    // TODO: Bug affects getBool method: https://github.com/robey/configgy/issues#issue/43
    def ignoringConfiggyException[T](f: => Option[T]): Option[T] =
      handling(classOf[ConfigException]) by{t => None} apply{f}

    cm.keys flatMap{ key =>
      ( cm.getConfigMap(key).map{m => JField(key, json(m))} orElse
        cm.getInt(key).map{x => JField(key, x)} orElse
        cm.getLong(key).map{x => JField(key, x)} orElse
        cm.getDouble(key).map{x => JField(key, x)} orElse
        ignoringConfiggyException(cm.getBool(key)).map{x => JField(key, x)} orElse
        cm.getString(key).flatMap{detectString(key) _}.map{x => JField(key, x)} orElse
        detectList(key)(cm.getList(key))
      ).iterator
    } toList
  }

  override def get[T <: Configuration : Manifest](name: String): Option[T] = {
    def extract(cm: ConfigMap) = {
      def recognitionError(t: Throwable) = {
        logger.debug("Failed to recognize configgy map %s as %s".format(cm, implicitly[Manifest[T]].erasure.getName), t)
        None
      }

      def recognize = {
        import net.liftweb.json.{Extraction, DefaultFormats}

        logger.trace("Loading configgy map %s as %s".format(cm, implicitly[Manifest[T]].erasure.getName))
        implicit val formats = DefaultFormats
        json(cm).extract[T]
      }

      catching(classOf[Throwable]) either recognize fold(recognitionError _ , x => Some(x))
    }

    configMap.getConfigMap(name) flatMap {cm => extract(cm)}
  }

  override def subscribe(f: => Unit) {
    configMap subscribe { configMap => f }
  }

  override def toString: String = "ConfiggyBackend "+configMap.getName()
}
