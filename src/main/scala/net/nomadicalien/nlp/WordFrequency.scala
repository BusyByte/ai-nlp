package net.nomadicalien.nlp

import scala.collection.mutable.ListBuffer
import scala.collection.mutable

object WordFrequency {

  case class WordRanking(rank : Int, word : String, probability : Double)

  val ESTIMATE_NUMBER_WORDS_IN_ENGLISH : Double = 1000000.0d
  val ONE_THIRD : Double = (1.0d / 3.0d) / 25.0d// = 0.01333 = 0.333 %
  val FIFTY_PERCENT : Double = (1.0d / 2.0d) / 100.0d //= 0.005 = 0.5 %

  private val sizeToWordRankingMap = createWordRankings()

  private def createWordRankings()  : Map[Int, List[WordRanking]] = {
    val tempMap = mutable.Map[Int, ListBuffer[WordRanking]]()
    addWordRanking(1, "the", ONE_THIRD, tempMap)
    addWordRanking(2, "be", ONE_THIRD, tempMap)
    addWordRanking(3, "to", ONE_THIRD, tempMap)
    addWordRanking(4, "of", ONE_THIRD, tempMap)
    addWordRanking(5, "and", ONE_THIRD, tempMap)
    addWordRanking(6, "a", ONE_THIRD, tempMap)
    addWordRanking(7, "in", ONE_THIRD, tempMap)
    addWordRanking(8, "that", ONE_THIRD, tempMap)
    addWordRanking(9, "have", ONE_THIRD, tempMap)
    addWordRanking(10, "i", ONE_THIRD, tempMap)
    addWordRanking(11, "it", ONE_THIRD, tempMap)
    addWordRanking(12, "for", ONE_THIRD, tempMap)
    addWordRanking(13, "not", ONE_THIRD, tempMap)
    addWordRanking(14, "on", ONE_THIRD, tempMap)
    addWordRanking(15, "with", ONE_THIRD, tempMap)
    addWordRanking(16, "he", ONE_THIRD, tempMap)
    addWordRanking(17, "as", ONE_THIRD, tempMap)
    addWordRanking(18, "you", ONE_THIRD, tempMap)
    addWordRanking(19, "do", ONE_THIRD, tempMap)
    addWordRanking(20, "at", ONE_THIRD, tempMap)
    addWordRanking(21, "this", ONE_THIRD, tempMap)
    addWordRanking(22, "but", ONE_THIRD, tempMap)
    addWordRanking(23, "his", ONE_THIRD, tempMap)
    addWordRanking(24, "by", ONE_THIRD, tempMap)
    addWordRanking(25, "from", ONE_THIRD, tempMap)
    addWordRanking(26, "they", FIFTY_PERCENT, tempMap)
    addWordRanking(27, "we", FIFTY_PERCENT, tempMap)
    addWordRanking(28, "say", FIFTY_PERCENT, tempMap)
    addWordRanking(29, "her", FIFTY_PERCENT, tempMap)
    addWordRanking(30, "she", FIFTY_PERCENT, tempMap)
    addWordRanking(31, "or", FIFTY_PERCENT, tempMap)
    addWordRanking(32, "an", FIFTY_PERCENT, tempMap)
    addWordRanking(33, "will", FIFTY_PERCENT, tempMap)
    addWordRanking(34, "my", FIFTY_PERCENT, tempMap)
    addWordRanking(35, "one", FIFTY_PERCENT, tempMap)
    addWordRanking(36, "all", FIFTY_PERCENT, tempMap)
    addWordRanking(37, "would", FIFTY_PERCENT, tempMap)
    addWordRanking(38, "there", FIFTY_PERCENT, tempMap)
    addWordRanking(39, "their", FIFTY_PERCENT, tempMap)
    addWordRanking(40, "what", FIFTY_PERCENT, tempMap)
    addWordRanking(41, "so", FIFTY_PERCENT, tempMap)
    addWordRanking(42, "up", FIFTY_PERCENT, tempMap)
    addWordRanking(43, "out", FIFTY_PERCENT, tempMap)
    addWordRanking(44, "if", FIFTY_PERCENT, tempMap)
    addWordRanking(45, "about", FIFTY_PERCENT, tempMap)
    addWordRanking(46, "who", FIFTY_PERCENT, tempMap)
    addWordRanking(47, "get", FIFTY_PERCENT, tempMap)
    addWordRanking(48, "which", FIFTY_PERCENT, tempMap)
    addWordRanking(49, "go", FIFTY_PERCENT, tempMap)
    addWordRanking(50, "me", FIFTY_PERCENT, tempMap)
    addWordRanking(51, "when", FIFTY_PERCENT, tempMap)
    addWordRanking(52, "make", FIFTY_PERCENT, tempMap)
    addWordRanking(53, "can", FIFTY_PERCENT, tempMap)
    addWordRanking(54, "like", FIFTY_PERCENT, tempMap)
    addWordRanking(55, "time", FIFTY_PERCENT, tempMap)
    addWordRanking(56, "no", FIFTY_PERCENT, tempMap)
    addWordRanking(57, "just", FIFTY_PERCENT, tempMap)
    addWordRanking(58, "him", FIFTY_PERCENT, tempMap)
    addWordRanking(59, "know", FIFTY_PERCENT, tempMap)
    addWordRanking(60, "take", FIFTY_PERCENT, tempMap)
    addWordRanking(61, "person", FIFTY_PERCENT, tempMap)
    addWordRanking(62, "into", FIFTY_PERCENT, tempMap)
    addWordRanking(63, "year", FIFTY_PERCENT, tempMap)
    addWordRanking(64, "your", FIFTY_PERCENT, tempMap)
    addWordRanking(65, "good", FIFTY_PERCENT, tempMap)
    addWordRanking(66, "some", FIFTY_PERCENT, tempMap)
    addWordRanking(67, "could", FIFTY_PERCENT, tempMap)
    addWordRanking(68, "them", FIFTY_PERCENT, tempMap)
    addWordRanking(69, "see", FIFTY_PERCENT, tempMap)
    addWordRanking(70, "other", FIFTY_PERCENT, tempMap)
    addWordRanking(71, "than", FIFTY_PERCENT, tempMap)
    addWordRanking(72, "then", FIFTY_PERCENT, tempMap)
    addWordRanking(73, "now", FIFTY_PERCENT, tempMap)
    addWordRanking(74, "look", FIFTY_PERCENT, tempMap)
    addWordRanking(75, "only", FIFTY_PERCENT, tempMap)
    addWordRanking(76, "come", FIFTY_PERCENT, tempMap)
    addWordRanking(77, "its", FIFTY_PERCENT, tempMap)
    addWordRanking(78, "over", FIFTY_PERCENT, tempMap)
    addWordRanking(79, "think", FIFTY_PERCENT, tempMap)
    addWordRanking(80, "also", FIFTY_PERCENT, tempMap)
    addWordRanking(81, "back", FIFTY_PERCENT, tempMap)
    addWordRanking(82, "after", FIFTY_PERCENT, tempMap)
    addWordRanking(83, "use", FIFTY_PERCENT, tempMap)
    addWordRanking(84, "two", FIFTY_PERCENT, tempMap)
    addWordRanking(85, "how", FIFTY_PERCENT, tempMap)
    addWordRanking(86, "our", FIFTY_PERCENT, tempMap)
    addWordRanking(87, "work", FIFTY_PERCENT, tempMap)
    addWordRanking(88, "first", FIFTY_PERCENT, tempMap)
    addWordRanking(89, "well", FIFTY_PERCENT, tempMap)
    addWordRanking(90, "way", FIFTY_PERCENT, tempMap)
    addWordRanking(91, "even", FIFTY_PERCENT, tempMap)
    addWordRanking(92, "new", FIFTY_PERCENT, tempMap)
    addWordRanking(93, "want", FIFTY_PERCENT, tempMap)
    addWordRanking(94, "because", FIFTY_PERCENT, tempMap)
    addWordRanking(95, "any", FIFTY_PERCENT, tempMap)
    addWordRanking(96, "these", FIFTY_PERCENT, tempMap)
    addWordRanking(97, "give", FIFTY_PERCENT, tempMap)
    addWordRanking(98, "day", FIFTY_PERCENT, tempMap)
    addWordRanking(99, "most", FIFTY_PERCENT, tempMap)
    addWordRanking(100, "us", FIFTY_PERCENT, tempMap)
    tempMap.mapValues[List[WordRanking]](_.toList).toMap
  }




  private def addWordRanking(ranking : Int, word : String, probability : Double,  mapToUse  : mutable.Map[Int, ListBuffer[WordRanking]]) {
    val length = word.length()

    val rankingList = mapToUse.getOrElseUpdate(length, ListBuffer())
    rankingList += WordRanking(ranking, word, probability)
  }

  def getRankingList(length : Int) : List[WordRanking] = {
    sizeToWordRankingMap.getOrElse(length, ListBuffer()).toList
  }

  /*
   *
   * Nouns

    time
    person
    year
    way
    day
    thing
    man
    world
    life
    hand
    part
    child
    eye
    woman
    place
    work
    week
    case
    point
    government
    company
    number
    group
    problem
    fact


[edit] Verbs

    be
    have
    do
    say
    get
    make
    go
    know
    take
    see
    come
    think
    look
    want
    give
    use
    find
    tell
    ask
    work
    seem
    feel
    try
    leave
    call


[edit] Adjectives

    long
    little
    own
    other
    old
    right
    big
    high
    different
    small
    large
    next
    early
    young
    important
    few
    public
    bad
    same
    able
    wonderful


[edit] Prepositions

    to
    of
    in
    for
    on
    with
    at
    by
    from
    up
    about
    into
    over
    after

   *
   */
}
