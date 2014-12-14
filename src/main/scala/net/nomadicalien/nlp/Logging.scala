package net.nomadicalien.nlp

import org.apache.logging.log4j.LogManager

/**
 * User: Shawn Garner
 * Created: 4/21/13 9:19 AM
 */
trait Logging {
  lazy val logger = LogManager.getLogger(this.getClass)
}
