package net.nomadicalien.nlp

import scala.collection.mutable
import scala.io.BufferedSource

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

  val wordSizeToWords : Map[Int, Set[String]] = loadWords


  private def loadWords : Map[Int, Set[String]] = {
    val accumulateSizeToWords = mutable.Map[Int, mutable.Set[String]]()
    wordFiles.foreach {
      fileName =>
        loadWordsFromFile(fileName).foreach {
          case (key : Int, theSetToAdd : Set[String]) =>
           val setToAddTo = accumulateSizeToWords.getOrElseUpdate(key, mutable.Set[String]())
           setToAddTo ++= theSetToAdd
        }
    }
    accumulateSizeToWords.mapValues(_.toSet).toMap
  }

  private def loadWordsFromFile(fileName : String) : Map[Int, Set[String]] = {
    val accumulateSizeToWords = mutable.Map[Int, mutable.Set[String]]()
    val stream: BufferedSource = scala.io.Source.fromInputStream(Thread.currentThread().getContextClassLoader.getResourceAsStream(fileName), "ISO-8859-1")

    stream.getLines().foreach {
      readLine : String =>
      val trimmedLine = readLine.trim()
      val length: Int = trimmedLine.length
      if (length > 0) {
          val setToAddTo : mutable.Set[String] = accumulateSizeToWords.getOrElseUpdate(length, mutable.Set[String]())
          setToAddTo += trimmedLine
      }
    }
    stream.close()

    accumulateSizeToWords.mapValues(_.toSet).toMap
  }

  def findWord(wordToSearchFor : String) : Boolean = {
    val length = wordToSearchFor.length
    wordSizeToWords.get(length).exists(_.contains(wordToSearchFor))
  }

  def numberWordsOfSize(length : Int) : Int = {
    wordSizeToWords.get(length).map(_.size).getOrElse(0)
  }
}
