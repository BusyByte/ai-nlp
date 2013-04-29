package net.nomadicalien.nlp

import org.scalatest.junit.JUnitSuite
import org.junit.Test

/**
 * User: Shawn Garner
 * Created: 4/28/13 6:50 PM
 */
class KnownWordsTest extends JUnitSuite {
  @Test def verifyFindWord() {
     assert(KnownWords.findWord("hello"))
  }

  @Test def verifyNumberWordsOfSize() {
     assert(KnownWords.numberWordsOfSize(4) === 7414)
  }
}
