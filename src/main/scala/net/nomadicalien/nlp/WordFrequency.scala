package net.nomadicalien.nlp

import scala.collection.mutable.ListBuffer

/**
 * User: Shawn Garner
 * Created: 4/11/13 10:47 PM
 */
object WordFrequency {


  case class WordRanking(rank : Int, word : String, probability : Double)

  val ESTIMATE_NUMBER_WORDS_IN_ENGLISH : Double = 1000000.0d
  val ONE_THIRD : Double = (1.0d / 3.0d) / 25.0d// = 0.01333 = 0.333 %
  val FIFTY_PERCENT : Double = (1.0d / 2.0d) / 100.0d //= 0.005 = 0.5 %

  val sizeToWordRankingMap = scala.collection.mutable.Map[Integer, ListBuffer[WordRanking]]()


  addWordRanking(1, "the", ONE_THIRD)
  addWordRanking(2, "be", ONE_THIRD)
  addWordRanking(3, "to", ONE_THIRD)
  addWordRanking(4, "of", ONE_THIRD)
  addWordRanking(5, "and", ONE_THIRD)
  addWordRanking(6, "a", ONE_THIRD)
  addWordRanking(7, "in", ONE_THIRD)
  addWordRanking(8, "that", ONE_THIRD)
  addWordRanking(9, "have", ONE_THIRD)
  addWordRanking(10, "i", ONE_THIRD)
  addWordRanking(11, "it", ONE_THIRD)
  addWordRanking(12, "for", ONE_THIRD)
  addWordRanking(13, "not", ONE_THIRD)
  addWordRanking(14, "on", ONE_THIRD)
  addWordRanking(15, "with", ONE_THIRD)
  addWordRanking(16, "he", ONE_THIRD)
  addWordRanking(17, "as", ONE_THIRD)
  addWordRanking(18, "you", ONE_THIRD)
  addWordRanking(19, "do", ONE_THIRD)
  addWordRanking(20, "at", ONE_THIRD)
  addWordRanking(21, "this", ONE_THIRD)
  addWordRanking(22, "but", ONE_THIRD)
  addWordRanking(23, "his", ONE_THIRD)
  addWordRanking(24, "by", ONE_THIRD)
  addWordRanking(25, "from", ONE_THIRD)
  addWordRanking(26, "they", FIFTY_PERCENT)
  addWordRanking(27, "we", FIFTY_PERCENT)
  addWordRanking(28, "say", FIFTY_PERCENT)
  addWordRanking(29, "her", FIFTY_PERCENT)
  addWordRanking(30, "she", FIFTY_PERCENT)
  addWordRanking(31, "or", FIFTY_PERCENT)
  addWordRanking(32, "an", FIFTY_PERCENT)
  addWordRanking(33, "will", FIFTY_PERCENT)
  addWordRanking(34, "my", FIFTY_PERCENT)
  addWordRanking(35, "one", FIFTY_PERCENT)
  addWordRanking(36, "all", FIFTY_PERCENT)
  addWordRanking(37, "would", FIFTY_PERCENT)
  addWordRanking(38, "there", FIFTY_PERCENT)
  addWordRanking(39, "their", FIFTY_PERCENT)
  addWordRanking(40, "what", FIFTY_PERCENT)
  addWordRanking(41, "so", FIFTY_PERCENT)
  addWordRanking(42, "up", FIFTY_PERCENT)
  addWordRanking(43, "out", FIFTY_PERCENT)
  addWordRanking(44, "if", FIFTY_PERCENT)
  addWordRanking(45, "about", FIFTY_PERCENT)
  addWordRanking(46, "who", FIFTY_PERCENT)
  addWordRanking(47, "get", FIFTY_PERCENT)
  addWordRanking(48, "which", FIFTY_PERCENT)
  addWordRanking(49, "go", FIFTY_PERCENT)
  addWordRanking(50, "me", FIFTY_PERCENT)
  addWordRanking(51, "when", FIFTY_PERCENT)
  addWordRanking(52, "make", FIFTY_PERCENT)
  addWordRanking(53, "can", FIFTY_PERCENT)
  addWordRanking(54, "like", FIFTY_PERCENT)
  addWordRanking(55, "time", FIFTY_PERCENT)
  addWordRanking(56, "no", FIFTY_PERCENT)
  addWordRanking(57, "just", FIFTY_PERCENT)
  addWordRanking(58, "him", FIFTY_PERCENT)
  addWordRanking(59, "know", FIFTY_PERCENT)
  addWordRanking(60, "take", FIFTY_PERCENT)
  addWordRanking(61, "person", FIFTY_PERCENT)
  addWordRanking(62, "into", FIFTY_PERCENT)
  addWordRanking(63, "year", FIFTY_PERCENT)
  addWordRanking(64, "your", FIFTY_PERCENT)
  addWordRanking(65, "good", FIFTY_PERCENT)
  addWordRanking(66, "some", FIFTY_PERCENT)
  addWordRanking(67, "could", FIFTY_PERCENT)
  addWordRanking(68, "them", FIFTY_PERCENT)
  addWordRanking(69, "see", FIFTY_PERCENT)
  addWordRanking(70, "other", FIFTY_PERCENT)
  addWordRanking(71, "than", FIFTY_PERCENT)
  addWordRanking(72, "then", FIFTY_PERCENT)
  addWordRanking(73, "now", FIFTY_PERCENT)
  addWordRanking(74, "look", FIFTY_PERCENT)
  addWordRanking(75, "only", FIFTY_PERCENT)
  addWordRanking(76, "come", FIFTY_PERCENT)
  addWordRanking(77, "its", FIFTY_PERCENT)
  addWordRanking(78, "over", FIFTY_PERCENT)
  addWordRanking(79, "think", FIFTY_PERCENT)
  addWordRanking(80, "also", FIFTY_PERCENT)
  addWordRanking(81, "back", FIFTY_PERCENT)
  addWordRanking(82, "after", FIFTY_PERCENT)
  addWordRanking(83, "use", FIFTY_PERCENT)
  addWordRanking(84, "two", FIFTY_PERCENT)
  addWordRanking(85, "how", FIFTY_PERCENT)
  addWordRanking(86, "our", FIFTY_PERCENT)
  addWordRanking(87, "work", FIFTY_PERCENT)
  addWordRanking(88, "first", FIFTY_PERCENT)
  addWordRanking(89, "well", FIFTY_PERCENT)
  addWordRanking(90, "way", FIFTY_PERCENT)
  addWordRanking(91, "even", FIFTY_PERCENT)
  addWordRanking(92, "new", FIFTY_PERCENT)
  addWordRanking(93, "want", FIFTY_PERCENT)
  addWordRanking(94, "because", FIFTY_PERCENT)
  addWordRanking(95, "any", FIFTY_PERCENT)
  addWordRanking(96, "these", FIFTY_PERCENT)
  addWordRanking(97, "give", FIFTY_PERCENT)
  addWordRanking(98, "day", FIFTY_PERCENT)
  addWordRanking(99, "most", FIFTY_PERCENT)
  addWordRanking(100, "us", FIFTY_PERCENT)


  def addWordRanking(ranking : Int, word : String, probability : Double) {
    val length = word.length()

    val rankingList = getOrCreateRankingList(length)
    rankingList += WordRanking(ranking, word, probability)
  }

  def getOrCreateRankingList(length : Int) : ListBuffer[WordRanking] =  {
    sizeToWordRankingMap.getOrElseUpdate(length, ListBuffer())
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
