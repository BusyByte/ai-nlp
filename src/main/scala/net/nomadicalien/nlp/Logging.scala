package net.nomadicalien.nlp

import org.apache.logging.log4j.LogManager

trait Logging {
  implicit lazy val logger = LogManager.getLogger(this.getClass)
}
