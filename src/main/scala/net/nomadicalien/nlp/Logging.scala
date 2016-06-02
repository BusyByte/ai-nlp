package net.nomadicalien.nlp

import org.apache.logging.log4j.LogManager

trait Logging {
  lazy val logger = LogManager.getLogger(this.getClass)
}
