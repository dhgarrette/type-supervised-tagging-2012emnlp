package dhgarrette.typesupervisedtagging.minmodel

import scala.collection.generic.Growable
import scala.collection.generic.Shrinkable
import scala.collection.mutable.Buffer
import scala.collection.mutable.{Set => MSet}
import scala.collection.mutable.{Map => MMap}
import scala.collection.{Map => CMap}
import scala.collection.{Set => CSet}

import org.apache.commons.logging.LogFactory
import org.apache.log4j.Level
import org.apache.log4j.Logger

import dhgarrette.typesupervisedtagging.minmodel.ModelMinimizer.LOG
import dhgarrette.typesupervisedtagging.util.Time.time
import opennlp.scalabha.util.CollectionUtils._

/**
 * Greedily search for a minimal set of tags that spans every sentence.
 */
class ModelMinimizer(
  startEndTag: String,
  tagBigramSelector: TagBigramSelector,
  getHolesOptimization: Boolean = true,
  pctSentToComplete: Int = 100) {

  /**
   * Find the minimal set of tag bigrams by iterating through the dataset in batches.
   */
  def minTagPath(sentences: Iterable[IndexedSeq[String]], fullTagDict: Map[String, Set[String]], batchSize: Int): (Map[String, Set[String]], List[List[(String, String)]]) = {
    val (allChosens, allTaggings) =
      sentences.grouped(batchSize).zipWithIndex.foldLeft(((List.empty[Map[String, Set[String]]], List.empty[List[List[(String, String)]]]), Set.empty[String])) {
        case (((chosens, taggings), chosenTags), (batch, i)) =>
          val (chosen, tagging) = time("minTagPath batch %d".format(i), minTagPath(batch, fullTagDict, chosenTags), LOG)
          ((chosen :: chosens, tagging :: taggings), chosenTags | chosen.keySet | chosen.values.flatten.toSet)
      }._1
    (allChosens.flatten.groupByKey.mapValuesStrict(_.flatten.toSet), allTaggings.reverse.flatten)
  }

  /**
   * Find the minimal set of tag bigrams across the sentences.
   */
  def minTagPath(sentences: Iterable[IndexedSeq[String]], tagDict: Map[String, Set[String]]): (Map[String, Set[String]], List[List[(String, String)]]) =
    minTagPath(sentences, tagDict, Set[String]())

  /**
   * Find the minimal set of tag bigrams across the sentences with a starting point for the set of chosen tags.
   */
  def minTagPath(sentences: Iterable[IndexedSeq[String]], tagDict: Map[String, Set[String]], chosenTagsStart: Set[String]): (Map[String, Set[String]], List[List[(String, String)]]) = {
    val newSentences = sentences.map("START_WORD" +: _ :+ "END_WORD").map(_.toList).toList
    val sentenceLengths = newSentences.map(_.size)
    val validTagsForUnknownWords = tagDict.flatMap(_._2.toList).counts.toList.sortBy(-_._2).map(_._1) /*.take(50)*/ .toSet
    val newTagDict = (tagDict.toMap.filter(_._2.nonEmpty) + ("START_WORD" -> Set(startEndTag), "END_WORD" -> Set(startEndTag))).withDefaultValue(validTagsForUnknownWords)

    // candidate tag bigrams and associated word indices
    val tagBigramsToWordBigrams = allCoverSubsets(newSentences, newTagDict) // mapping from tag bigrams to word pairs covered
    val tagBigramsToCoveredWords = tagBigramsToWordBigrams.mapValuesStrict(_.flatten.toMap) // mapping from tag bigrams to a set of words covered

    var paths = List.fill[Option[List[String]]](newSentences.size)(None) // a tag-path through each sentence.  None until a path is found. 
    var wordTagsSeen = MSet.empty[(String, String)] // a set of all word/tag pairs found in 'paths' [for fast access]

    //
    // Stage1
    //
    var cand = MMap() ++= tagBigramsToCoveredWords.mapValuesStrict(x => MWordList(MMap() ++= x)) // convert tagBigramsToCoveredWords to full mutability
    val chosen = new MBigrams // tag bigrams selected for use
    val chosenTags = MSet() ++= chosenTagsStart // all tags in chosen bigrams [for fast access]
    if (LOG.isInfoEnabled) LOG.info("%d sentences, %d candidate tag bigrams, %d words".format(newSentences.size, cand.size, cand.flatMap(_._2.get).size))

    time("phase1", {
      while (cand.nonEmpty) {
        // pick the most frequent tag bigram
        // break ties by minimizing the addition of new tags
        val (e, coveredWords) = selectTagBigram(cand, wordTagsSeen, chosenTags, tagBigramsToWordBigrams)

        chosen += e
        chosenTags += (e._1, e._2)
        cand -= e // remove the tag bigram from the candidate map
        cand.foreach(_._2.get --= coveredWords.wordSet.keys) // remove covered words from each entry in the candidate map
        cand.retain((k, v) => v.get.nonEmpty) // remove tag bigrams from the candidate map that don't cover anything

        paths = (newSentences zipEqual paths).map {
          case (words, None) =>
            findFullPath(words, chosen, newTagDict) match {
              case Some(newPath) =>
                wordTagsSeen ++= (words zipEqual newPath).toSet
                Some(newPath)
              case None => None
            }
          case (_, p) => p
        }

        if (LOG.isInfoEnabled && chosen.size % 100 == 0)
          LOG.info("  %d chosen: %d sentences remaining, %d words remaining, %d candidates remaining".format(chosen.size, paths.count(_.isEmpty), cand.flatMap(_._2.get).size, cand.size))
      }
      LOG.info("  %d chosen".format(chosen.size))
    }, LOG)

    //
    // Stage2
    //
    val bCand = new MBigrams ++= (tagBigramsToCoveredWords.keySet -- chosen.toSet) // candidate tag bigrams; all unchosen bigrams
    val startsEnds = makeGraph(chosen, newSentences, sentenceLengths, newTagDict).map(Option(_))
    val maxIncompleteSentences = paths.size * (100 - pctSentToComplete) / 100

    time("phase2", {
      LOG.info("  %d chosen: %d sentences remaining, %d candidates remaining".format(chosen.size, paths.count(_.isEmpty), bCand.size))
      while (paths.countCompare(_.isEmpty, maxIncompleteSentences) > 0 && bCand.nonEmpty) {
        if (getHolesOptimization)
          for ((p, i) <- paths.zipWithIndex if p.isDefined) startsEnds(i) = None //remove completed sentences from startsEnds

        // pick the tag bigram that fills the most holes
        // break ties by minimizing the addition of new tags
        val bigramsAndHoleTuplesBySent = getHoles(newSentences, startsEnds, bCand)
        val bigramsAndHoleTuples = bigramsAndHoleTuplesBySent.mapValuesStrict(x => HolesList(x.flatMap { case (sid, wordBigrams) => wordBigrams.map(_.map { case (wid, wordTag) => (sid, wid) -> wordTag }) }))
        val (e, _) = selectTagBigram(bigramsAndHoleTuples, wordTagsSeen, chosenTags, tagBigramsToWordBigrams)

        chosen += e
        chosenTags += (e._1, e._2)
        bCand -= e

        for (
          (sid, wids) <- bigramsAndHoleTuplesBySent(e);
          (sentStarts, sentEnds) <- startsEnds(sid).toSeq;
          List((wid1, _), (wid2, _)) <- wids
        ) {
          sentStarts(wid1) += e._1
          sentEnds(wid2) += e._2
        }

        paths = (newSentences zipEqual paths).map {
          case (words, None) =>
            findFullPath(words, chosen, newTagDict) match {
              case Some(newPath) =>
                wordTagsSeen ++= (words zipEqual newPath).toSet
                Some(newPath)
              case None => None
            }
          case (_, p) => p
        }

        if (chosen.size % 100 == 0)
          LOG.info("  %d chosen: %d sentences remaining, %d candidates remaining".format(chosen.size, paths.count(_.isEmpty), bCand.size))
      }
      LOG.info("  %d chosen: %d sentences remaining, %d candidates remaining".format(chosen.size, paths.count(_.isEmpty), bCand.size))
    }, LOG)

    //
    // Return chosen tag bigrams and the minimal tagging
    //
    val tagging =
      (newSentences zipEqual paths).flatMap {
        case (sent, Some(path)) => Some((sent zipEqual path).tail.dropRight(1))
        case (sent, None) => None
      }
    (chosen.toSet.groupByKey, tagging)
  }

  def selectTagBigram(
    cand: CMap[(String, String), StructuredWordList],
    wordTagsSeen: CSet[(String, String)],
    chosenTags: CSet[String],
    tagBigramsToWordBigrams: Map[(String, String), Iterable[List[((Int, Int), (String, String))]]]) = {

    val best = tagBigramSelector.selectTagBigram(cand, wordTagsSeen, chosenTags, tagBigramsToWordBigrams)
    val (bigram, coveredWords) = best.head // pick randomly from those that are equally the best

    if (LOG.isDebugEnabled) {
      LOG.debug("        chose: %s: covers %d: %s".format(
        bigram, coveredWords.size,
        tagBigramsToWordBigrams(bigram).map { case Seq((_, (w1, _)), (_, (w2, _))) => (w1, w2) }.toSet.toList.sorted.take(5)))
    }

    (bigram, coveredWords)
  }

  /**
   * Map tag bigrams, to the word bigrams they cover.  Word bigrams are
   * represented as a list of size two.  Words are represented as a
   * pair of (sentence id, word id) and a word/tag pair.
   */
  private def allCoverSubsets(newSentences: Iterable[Seq[String]], newTagDict: Map[String, Set[String]]) = {
    val tagsBigramsWordIndexPairs =
      for (
        (words, sid) <- newSentences.zipWithIndex;
        Seq((word1, wid1), (word2, wid2)) <- words.zipWithIndex.sliding(2);
        label1 <- newTagDict(word1);
        label2 <- newTagDict(word2)
      ) yield (label1, label2) -> List((sid, wid1) -> (word1, label1), (sid, wid2) -> (word2, label2))
    tagsBigramsWordIndexPairs.groupByKey
  }

  /**
   * For each word, list tags that start/end bigrams on that word.
   */
  private def makeGraph(chosen: MBigrams, newSentences: Iterable[Seq[String]], sentenceLengths: Iterable[Int], newTagDict: Map[String, Set[String]]) = {
    val startsEnds = (for (sl <- sentenceLengths) yield (Vector.fill(sl - 1)(MSet[String]()) :+ MSet(startEndTag), MSet(startEndTag) +: Vector.fill(sl - 1)(MSet[String]()))).toBuffer

    for (
      (words, (sentStarts, sentEnds)) <- newSentences zipEqual startsEnds;
      Seq((word1, (starts1, _)), (word2, (_, ends2))) <- (words zipEqual (sentStarts zipEqual sentEnds)).sliding(2);
      label1 <- newTagDict(word1);
      label2 <- newTagDict(word2)
    ) {
      if (chosen.contains(label1, label2)) {
        starts1 += label1
        ends2 += label2
      }
    }

    startsEnds
  }

  /**
   * Find the subset of candidates that would fill holes in the graph.  A hole
   * is a candidate edge that connects an 'end' to a 'start'; ie, it connects
   * two disconnected edges to make a path.
   */
  private def getHoles(
    newSentences: Iterable[Seq[String]],
    allStartsEnds: Buffer[_ <: Option[(Vector[CSet[String]], Vector[CSet[String]])]],
    cand: MBigrams) = {

    val tagBigramsToHoles =
      for (
        ((startsEndsOpt, sid), sentence) <- allStartsEnds.zipWithIndex.par zipEqual newSentences;
        (sentStarts, sentEnds) <- startsEndsOpt.toSeq;
        Seq((((_, ends1), word1), wid1), (((starts2, _), word2), wid2)) <- ((sentStarts zipEqual sentEnds zipEqual sentence).zipWithIndex).sliding(2); // each word token pair along with the tags bigrams that end on the first word and the tag bigrams that start on the second word
        label1 <- ends1;
        candStarts <- cand.get(label1).toSeq;
        label2 <- starts2 & candStarts
      ) yield (label1, label2) -> (sid, List((wid1, (word1, label1)), (wid2, (word2, label2))))

    tagBigramsToHoles.seq
      .groupByKey
      .mapValuesStrict(_.groupByKey)
  }

  private def hasFullPath(words: List[String], chosen: MBigrams, newTagDict: Map[String, Set[String]]): Boolean = {
    var tags = Set(startEndTag)
    for (w <- words.drop(1)) {
      tags =
        for (
          start <- tags;
          chosenEnds <- chosen.get(start).toSeq;
          end <- newTagDict(w) & chosenEnds
        ) yield end
    }
    tags.contains(startEndTag)
  }

  private def findFullPath(words: List[String], chosen: MBigrams, newTagDict: Map[String, Set[String]]): Option[List[String]] = {
    if (hasFullPath(words, chosen, newTagDict))
      _findFullPath(words.tail, startEndTag, chosen, newTagDict)
    else
      None
  }

  private def _findFullPath(words: List[String], label: String, chosen: MBigrams, newTagDict: Map[String, Set[String]]): Option[List[String]] = {
    words match {
      case Nil =>
        if (label == startEndTag)
          Some(List(label))
        else
          throw new RuntimeException("should never happen")
      case _ if !chosen.contains(label) =>
        None
      case _ =>
        val nextLabels =
          words match {
            case word :: _ => newTagDict(word)
            case Nil => Set(startEndTag)
          }

        (nextLabels & chosen(label)).foldLeft[Option[List[String]]](None)((pathFound, nextLabel) =>
          if (pathFound.isDefined)
            pathFound
          else
            _findFullPath(words.tail, nextLabel, chosen, newTagDict) match {
              case Some(list) => Some(label :: list)
              case _ => None
            })
    }
  }
}

object ModelMinimizer {
  val LOG = LogFactory.getLog(ModelMinimizer.getClass)

  def main(args: Array[String]): Unit = {
    val sent1 = Vector(1, 2, 3, 4).map("w" + _)
    val sent2 = Vector(1, 4, 1, 4).map("w" + _)
    val sent3 = Vector(2, 2, 2, 2).map("w" + _)
    val tagDict = Map(
      1 -> Set(1, 3),
      2 -> Set(2),
      3 -> Set(1, 2),
      4 -> Set(2, 3))
      .map { case (k, v) => ("w" + k, v.map("t" + _)) }

    Logger.getRootLogger.setLevel(Level.DEBUG)

    println(
      new ModelMinimizer(
        "<END>",
        new CoverageMaximizingTagBigramSelector(new PassthroughTagBigramSelector))
        .minTagPath(List(sent1), tagDict))
    //    println(new ModelMinimizer("<END>").minTagPath(List(sent2), tagDict))
    //    println(new ModelMinimizer("<END>").minTagPath(List(sent2, sent1), tagDict))
    //    println(new ModelMinimizer("<END>").minTagPath(List(sent3, sent1), tagDict))

    //    val labeled = (TaggedFile("data/s19-21.pos")).toList
    //    val fullTagDict = new opennlp.scalabha.tag.hmm.support.SimpleTagDictFactory().make(labeled)
    //    val subsetLabeled = labeled.take(5)
    //    val subsetRaw = subsetLabeled.map(_.map(_._1).toIndexedSeq)
    //    new ModelMinimizer("<END>").minTagPath(subsetRaw, fullTagDict).foreach(println)

  }

  object TaggedFile {
    def apply(filename: String) =
      io.Source.fromFile(filename).getLines
        .map(_.trim
          .split(" ")
          .map(_.split("\\|").toSeq.toTuple2).toIndexedSeq)
  }

}

abstract class StructuredWordList {
  def size: Int
  def wordSet: CMap[(Int, Int), (String, String)]
}

case class MWordList(get: MMap[(Int, Int), (String, String)]) extends StructuredWordList {
  override def size = get.size
  override def wordSet = get
}

case class HolesList(get: Iterable[List[((Int, Int), (String, String))]]) extends StructuredWordList {
  override def size = get.size
  override def wordSet = get.flatten.toMap
}

class MBigrams() extends Iterable[(String, String)] with Growable[(String, String)] with Shrinkable[(String, String)] {
  private val m = MMap[String, MSet[String]]()
  override def iterator() = m.iterator.flatMap { case (k, vs) => vs.map((k, _)) }
  override def +=(bigram: (String, String)) = { m.getOrElseUpdate(bigram._1, MSet[String]()) += bigram._2; this }
  override def -=(bigram: (String, String)) = { m.get(bigram._1).foreach(_ -= bigram._2); this }
  override def size(): Int = m.map(_._2.size).sum
  def contains(a: String): Boolean = m.contains(a)
  def contains(a: String, b: String): Boolean = m.get(a).map(_.contains(b)) match { case Some(b) => b; case _ => false }
  def apply(a: String): MSet[String] = m(a)
  def get(a: String): Option[MSet[String]] = m.get(a)
  def getOrElse(a: String, default: => MSet[String]): MSet[String] = m.getOrElse(a, default)
  def getDefault(a: String): MSet[String] = this.getOrElse(a, MSet.empty)
  override def clear(): Unit = m.clear()
}
