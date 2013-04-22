package net.nomadicalien.nlp

import org.apache.commons.lang3.time.StopWatch
import org.apache.log4j.Logger

/**
 * @author Shawn Garner
 */
object App extends Logging {
  val stopwatch = new StopWatch()

  def main(args : Array[String]) {
    stopwatch.start()
    val stringToDecode = "Esp qtcde nzyqpcpynp zy esp ezatn zq Lcetqtntlw Tyepwwtrpynp hld spwo le Olcexzfes Nzwwprp ty estd jplc.".toLowerCase()
    val nlp1 = new NLP2(stringToDecode)
    nlp1.process
    stopwatch.stop()
    logger.info("took ${(stopwatch.getTime() / 1000)} seconds")
  }



}
