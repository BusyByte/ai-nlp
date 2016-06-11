package net.nomadicalien.nlp.actor

import com.typesafe.config.ConfigFactory


object Config {
  lazy val conf = ConfigFactory.load()

  private[this] def safeGetConfigValue[A](key: String, f: String => A): Option[A] = {
    if (conf.hasPathOrNull(key)) {
      if (conf.getIsNull(key)) {
        None
      } else {
        Some(f(key))
      }
    } else {
      None
    }
  }

  def getInt(key: String) = safeGetConfigValue(key, conf.getInt)
  def getString(key: String) = safeGetConfigValue(key, conf.getString)
}
