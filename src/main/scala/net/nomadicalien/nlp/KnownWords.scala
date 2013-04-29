package net.nomadicalien.nlp

import scala.collection.mutable
import java.io.{Reader, BufferedReader, InputStreamReader}
import scala.util.parsing.input.StreamReader
import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource

/**
 * User: Shawn Garner
 * Created: 4/21/13 8:52 PM
 */
object KnownWords {
  val wordFiles = List(
    "english-words.10.words",
    "english-words.20.words",
    "english-words.35.words",
    "english-words.50.words",
    "english-words.55.words",
    "english-words.60.words",
    "english-words.70.words",
    "english-words.80.words",
    "english-words.95.words",
    "american-words.10.words",
    "american-words.20.words",
    "american-words.35.words",
    "american-words.40.words",
    "american-words.50.words",
    "american-words.55.words",
    "american-words.60.words",
    "american-words.70.words",
    "american-words.80.words",
    "american-words.95.words"
  )

  val wordSizeToWords = mutable.Map[Integer, ListBuffer[String]]()

   loadWords


  private def loadWords {
    wordFiles.foreach(loadWordsFromFile(_))
  }

  private def loadWordsFromFile(fileName : String)  {
    val stream: BufferedSource = scala.io.Source.fromInputStream(Thread.currentThread().getContextClassLoader().getResourceAsStream(fileName), "ISO-8859-1")

    stream.getLines().foreach {
      readLine : String =>
      val trimmedLine = readLine.trim()
      val length: Int = trimmedLine.length
      if (length > 0) {
          val listToAddTo : ListBuffer[String] = wordSizeToWords.getOrElseUpdate(length, ListBuffer[String]())
          listToAddTo += trimmedLine
      }
    }
    stream.close()
  }


  def findWord(wordToSearchFor : String) : Boolean = {
    val length = wordToSearchFor.length
    val wordList = wordSizeToWords.getOrElse(length, ListBuffer[String]())

    wordList.contains(wordToSearchFor)
  }

  def numberWordsOfSize(length : Int) : Int = {
    val wordList = wordSizeToWords.getOrElse(length, ListBuffer[String]())
    wordList.size
  }
}
