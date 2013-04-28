package net.nomadicalien.nlp

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._

/**
 * User: Shawn Garner
 * Created: 4/28/13 10:01 AM
 */
class ProbFormatterTest extends JUnitSuite {

  @Test def verifyFormat() {
    val numberToFormat = 1.6666666666d
    assertEquals("1.66667", ProbFormatter.format(numberToFormat))
  }
}
