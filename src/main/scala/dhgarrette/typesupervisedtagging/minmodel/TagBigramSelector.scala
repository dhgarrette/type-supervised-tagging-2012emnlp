package dhgarrette.typesupervisedtagging.minmodel

import scala.collection.{ Map => CMap }
import scala.collection.{ Set => CSet }

import org.apache.commons.logging.LogFactory

/**
 * Select the next tag bigram from the available candidates.  Use three criteria:
 * 1) Maximize impact (most word coverage or holes filled)
 * 2) Minimize introduction of new word/tag pairs
 * 3) Minimize introduction of new tags
 * If there are ties after these three conditions, pick one randomly
 */
trait TagBigramSelector {
  val LOG = LogFactory.getLog("TagBigramSelector")

  def selectTagBigram(
    cand: CMap[(String, String), StructuredWordList],
    wordTagsSeen: CSet[(String, String)],
    chosenTags: CSet[String],
    tagBigramsToWordBigrams: Map[(String, String), Iterable[List[((Int, Int), (String, String))]]]): CMap[(String, String), StructuredWordList] //((String, String), StructuredWordList)
}

class PassthroughTagBigramSelector extends TagBigramSelector {
  override def selectTagBigram(
    cand: CMap[(String, String), StructuredWordList],
    wordTagsSeen: CSet[(String, String)],
    chosenTags: CSet[String],
    tagBigramsToWordBigrams: Map[(String, String), Iterable[List[((Int, Int), (String, String))]]]) = {

    cand

  }
}

class CoverageMaximizingTagBigramSelector(delegate: TagBigramSelector) extends TagBigramSelector {
  override def selectTagBigram(
    cand: CMap[(String, String), StructuredWordList],
    wordTagsSeen: CSet[(String, String)],
    chosenTags: CSet[String],
    tagBigramsToWordBigrams: Map[(String, String), Iterable[List[((Int, Int), (String, String))]]]) = {

    // maximize coverage
    val best =
      delegate.selectTagBigram(cand, wordTagsSeen, chosenTags, tagBigramsToWordBigrams)
        .groupBy(_._2.size).maxBy(_._1)._2

    if (LOG.isDebugEnabled && best.size > 1) {
      LOG.debug("            %d choices maximize coverage: %s".format(best.size, best.toList.map(_._1).sorted.take(10)))
      //for (((a, _), b) <- best1.mapTo { case (_, words) => (words.map(_._2) -- wordTagsSeen) })
      //  LOG.debug("                %s introduces %d: %s".format(a, b.size, b))
    }

    best
  }
}

class NewTagMinimizingTagBigramSelector(delegate: TagBigramSelector) extends TagBigramSelector {
  override def selectTagBigram(
    cand: CMap[(String, String), StructuredWordList],
    wordTagsSeen: CSet[(String, String)],
    chosenTags: CSet[String],
    tagBigramsToWordBigrams: Map[(String, String), Iterable[List[((Int, Int), (String, String))]]]) = {

    // minimize introduction of new tags
    val best =
      delegate.selectTagBigram(cand, wordTagsSeen, chosenTags, tagBigramsToWordBigrams)
        .groupBy { case ((t1, t2), _) => (Set(t1, t2) -- chosenTags).size }.minBy(_._1)._2

    if (LOG.isDebugEnabled && best.size > 1) {
      LOG.debug("            %d choices minimize new tags: %s".format(best.size, best.toList.map(_._1).sorted.take(10)))
    }

    best
  }
}

class NewWordTagPairMinimizingTagBigramSelector(delegate: TagBigramSelector) extends TagBigramSelector {
  override def selectTagBigram(
    cand: CMap[(String, String), StructuredWordList],
    wordTagsSeen: CSet[(String, String)],
    chosenTags: CSet[String],
    tagBigramsToWordBigrams: Map[(String, String), Iterable[List[((Int, Int), (String, String))]]]) = {

    // minimize introduction of new word/tag pairs
    val best =
      delegate.selectTagBigram(cand, wordTagsSeen, chosenTags, tagBigramsToWordBigrams)
        .groupBy { x => (x._2.wordSet.map(_._2).toSet -- wordTagsSeen).size }.minBy(_._1)._2

    if (LOG.isDebugEnabled && best.size > 1) {
      LOG.debug("            %d choices minimize word/tag: %s".format(best.size, best.toList.map(_._1).sorted.take(10)))
      //for (((a, _), b) <- best2.mapTo { case ((t1, t2), _) => (Set(t1, t2) -- chosenTags) })
      //  LOG.debug("                %s introduces %d: %s".format(a, b.size, b))
    }

    best
  }
}

class BetterTagBigramSelector(delegate: TagBigramSelector) extends TagBigramSelector {
  override def selectTagBigram(
    cand: CMap[(String, String), StructuredWordList],
    wordTagsSeen: CSet[(String, String)],
    chosenTags: CSet[String],
    tagBigramsToWordBigrams: Map[(String, String), Iterable[List[((Int, Int), (String, String))]]]) = {

    //TODO: What I really want is something that is not simply based  
    // on the presence of existing word/tag pairs, but on their probabilities.
    // Maybe take all the words that will be newly covered, along with their 
    // tags, calucate the probability of each given the current 'paths' dist,
    // and pick the edge that has the highest probability.  This will get us
    // picking edges that favor high word/tag probability words.
    delegate.selectTagBigram(cand, wordTagsSeen, chosenTags, tagBigramsToWordBigrams)

  }
}
