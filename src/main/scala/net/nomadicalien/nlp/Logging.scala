package net.nomadicalien.nlp

import org.apache.log4j.Logger

/**
 * User: Shawn Garner
 * Created: 4/21/13 9:19 AM
 */
trait Logging {
  lazy val logger = Logger.getLogger(this.getClass.getName)
}
