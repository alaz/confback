package com.osinka.confback

// Convenience implicits for bootstrap.confback.Boot
trait ConfigBootstrap {
  import backend._
  import net.lag.configgy.ConfigMap

  implicit def fromConfiggy(configMap: ConfigMap) = new ConfiggyBackend(configMap)
}
