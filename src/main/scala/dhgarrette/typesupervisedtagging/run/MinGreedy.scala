package dhgarrette.typesupervisedtagging.run

import scala.annotation.tailrec

import dhgarrette.typesupervisedtagging.minmodel.ModelMinimizer
import dhgarrette.typesupervisedtagging.run.MinGreedy.TaggerTrainCommand
import opennlp.scalabha.tag.hmm.SemisupervisedHmmTaggerTrainer
import opennlp.scalabha.tag.hmm.SupervisedHmmTaggerTrainer
import opennlp.scalabha.tag.hmm.UnsupervisedHmmTaggerTrainer
import opennlp.scalabha.tag.support.CondCountsTransformer
import opennlp.scalabha.tag.support.TagDictFactory
import opennlp.scalabha.tag.ScoreResults
import opennlp.scalabha.tag.Tagger
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum

class MinGreedy[Sym, Tag](
  taggerEvaluate: (Tagger[Sym, Tag], Map[Sym, Set[Tag]]) => ScoreResults[Sym, Tag] // evaluate a tagger and tag dict
  ) {

  val startEndTag = "<END>".asInstanceOf[Tag]
  def end(seq: IndexedSeq[Tag]) = startEndTag +: seq :+ startEndTag
  def makeGrammar(tagged: Iterable[IndexedSeq[(Sym, Tag)]]) = tagged.map(s => end(s.map(_._2)).sliding2).flatten.toSet.groupByKey

  def run(
    fullTagDict: Map[Sym, Set[Tag]],
    rawTrain: Iterable[IndexedSeq[Sym]],
    tagDictFactory: TagDictFactory[Sym, Tag],
    firstIterationTrainer: TaggerTrainCommandFactory[Sym, Tag],
    oddIterationTrainer: TaggerTrainCommandFactory[Sym, Tag],
    evenIterationTrainer: TaggerTrainCommandFactory[Sym, Tag],
    finalTrainerOpt: Option[TaggerTrainCommandFactory[Sym, Tag]],
    modelMinimizer: ModelMinimizer,
    pathTagsOnly: Boolean) {

    val (minChosen, minAutoTagged) = modelMinimizer.minTagPath(rawTrain.asInstanceOf[Iterable[IndexedSeq[String]]], fullTagDict.asInstanceOf[Map[String, Set[String]]]).asInstanceOf[(Map[Tag, Set[Tag]], List[List[(Sym, Tag)]])]
    val autoTagged = minAutoTagged.map(_.toIndexedSeq)

    val constrainedGrammar = if (pathTagsOnly) makeGrammar(autoTagged) else minChosen
    println("number of bigrams for use after minimization: " + constrainedGrammar.flattenOver.size)

    val (minGreedyTagger, results) =
      doIteration(1,
        firstIterationTrainer,
        oddIterationTrainer,
        evenIterationTrainer,
        constrainedGrammar,
        autoTagged,
        rawTrain,
        tagDictFactory,
        fullTagDict,
        1,
        Nil)

    finalTrainerOpt.foreach { finalTrainer =>
      val minGreedyTaggerAutoTagged = minGreedyTagger.tag(rawTrain)
      val constrainedGrammar = makeGrammar(minGreedyTaggerAutoTagged)
      val contrainedTagDict = tagDictFactory.make(minGreedyTaggerAutoTagged)
      println("Supervised training on MIN-GREEDY tagged output")
      val autosupervisedTagger = finalTrainer(None, fullTagDict, rawTrain).train(fullTagDict, null, minGreedyTaggerAutoTagged)
      taggerEvaluate(autosupervisedTagger, fullTagDict).toString.split("\n").map("      " + _).foreach(println)
    }
  }

  @tailrec
  private def doIteration(i: Int,
    odd: TaggerTrainCommandFactory[Sym, Tag],
    nextOdd: TaggerTrainCommandFactory[Sym, Tag],
    even: TaggerTrainCommandFactory[Sym, Tag],
    constrainedGrammar: Map[Tag, Set[Tag]],
    autoTagged: Iterable[IndexedSeq[(Sym, Tag)]],
    rawTrain: Iterable[IndexedSeq[Sym]],
    tagDictFactory: TagDictFactory[Sym, Tag],
    fullTagDict: Map[Sym, Set[Tag]],
    prevGrammarSize: Int,
    results: List[(List[String], ScoreResults[Sym, Tag])]): (Tagger[Sym, Tag], List[(List[String], ScoreResults[Sym, Tag])]) = {

    val (oddTagger, autoTaggedByOdd, _, oddConstrainedTagDict, oddResultString, oddResults) =
      iterationSide(i,
        "EM with grammar from previous + full dictionary",
        odd,
        Some(constrainedGrammar),
        fullTagDict,
        rawTrain,
        autoTagged,
        tagDictFactory,
        fullTagDict)

    val (evenTagger, autoTaggedByEven, newConstrainedGrammar, _, evnResultString, evnResults) =
      iterationSide(i + 1,
        "EM with full grammar + dictionary from previous",
        even,
        None,
        oddConstrainedTagDict,
        rawTrain,
        autoTaggedByOdd,
        tagDictFactory,
        fullTagDict)

    val grammarSize = newConstrainedGrammar.flattenOver.size
    println("grammarSize=%d, prevGrammarSize=%d, diff=%d, pct=%f".format(grammarSize, prevGrammarSize, math.abs(grammarSize - prevGrammarSize), math.abs(grammarSize - prevGrammarSize) * 100.0 / prevGrammarSize))

    if (math.abs(grammarSize - prevGrammarSize) * 100 / prevGrammarSize < 5) {
      (evenTagger, results)
    }
    else {
      doIteration(i + 2,
        nextOdd,
        nextOdd,
        even,
        newConstrainedGrammar,
        autoTaggedByEven,
        rawTrain,
        tagDictFactory,
        fullTagDict,
        grammarSize,
        results ++ List(
          (oddResultString, oddResults),
          (evnResultString, evnResults)))
    }
  }

  private def iterationSide(
    i: Int,
    desc: String,
    trainerFactory: TaggerTrainCommandFactory[Sym, Tag],
    constrainedGrammar: Option[Map[Tag, Set[Tag]]],
    tagDict: Map[Sym, Set[Tag]],
    rawTrain: Iterable[IndexedSeq[Sym]],
    autoTagged: Iterable[IndexedSeq[(Sym, Tag)]],
    tagDictFactory: TagDictFactory[Sym, Tag],
    fullTagDict: Map[Sym, Set[Tag]]) = {

    val tagger = trainerFactory(constrainedGrammar, tagDict, rawTrain).train(tagDict, rawTrain, autoTagged)
    val newAutoTagged = tagger.tag(rawTrain)
    val newConstrainedGrammar = makeGrammar(newAutoTagged)
    val newConstrainedTagDict = tagDictFactory.make(newAutoTagged)
    val results = taggerEvaluate(tagger, fullTagDict)

    val resultString = List(
      desc,
      "  Observed in tagging of raw data with EM-trained tagger: num tag bigrams = %d, tag dict size = %d".format(newConstrainedGrammar.flattenOver.size, newConstrainedTagDict.flattenOver.size),
      "    Test set eval; full grammar:")

    printResult(i, resultString, results)

    (tagger, newAutoTagged, newConstrainedGrammar, newConstrainedTagDict, resultString, results)
  }

  private def iterationSide_OLD(
    i: Int,
    desc: String,
    tagger: Tagger[Sym, Tag],
    tagDict: Map[Sym, Set[Tag]],
    rawTrain: Iterable[IndexedSeq[Sym]],
    tagDictFactory: TagDictFactory[Sym, Tag],
    fullTagDict: Map[Sym, Set[Tag]]) = {

    val autoTagged = tagger.tag(rawTrain)
    val constrainedGrammar = makeGrammar(autoTagged)
    val constrainedTagDict = tagDictFactory.make(autoTagged)
    val results = taggerEvaluate(tagger, fullTagDict)
    val resultString = List(
      desc,
      "  Observed in tagging of raw data with EM-trained tagger: num tag bigrams = %d, tag dict size = %d".format(constrainedGrammar.flattenOver.size, constrainedTagDict.flattenOver.size),
      "    Test set eval; full grammar:")

    printResult(i, resultString, results)

    (constrainedGrammar, constrainedTagDict, resultString, results)
  }

  private def printResult(i: Int, b: List[String], r: ScoreResults[Sym, Tag]) = {
    println
    println(i + ": ")
    b.foreach(println)
    r.toString.split("\n").map("    " + _).foreach(println)
  }

}

object MinGreedy {

  trait TaggerTrainCommand[Sym, Tag] {
    def train(
      tagDict: Map[Sym, Set[Tag]],
      rawTrainSequences: Iterable[IndexedSeq[Sym]],
      taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]): Tagger[Sym, Tag]
  }

  case class UnsupervisedTaggerTrainCommand[Sym, Tag](
    initialUnsupervisedEmissionDist: Tag => Sym => LogNum,
    estimatedTransitionCountsTransformer: CondCountsTransformer[Tag, Tag],
    estimatedEmissionCountsTransformer: CondCountsTransformer[Tag, Sym],
    startEndSymbol: Sym,
    startEndTag: Tag,
    maxIterations: Int = 50,
    minAvgLogProbChangeForEM: Double = 0.00001)
    extends TaggerTrainCommand[Sym, Tag] {

    override def train(
      tagDict: Map[Sym, Set[Tag]],
      rawTrainSequences: Iterable[IndexedSeq[Sym]],
      taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]): Tagger[Sym, Tag] = {

      val trainer =
        new UnsupervisedHmmTaggerTrainer(
          initialUnsupervisedEmissionDist,
          estimatedTransitionCountsTransformer,
          estimatedEmissionCountsTransformer,
          startEndSymbol,
          startEndTag,
          maxIterations,
          minAvgLogProbChangeForEM)
      trainer.trainUnsupervised(tagDict, rawTrainSequences)
    }
  }

  case class SemisupervisedTaggerTrainCommand[Sym, Tag](
    initialTransitionCountsTransformer: CondCountsTransformer[Tag, Tag],
    initialEmissionCountsTransformer: CondCountsTransformer[Tag, Sym],
    estimatedTransitionCountsTransformer: CondCountsTransformer[Tag, Tag],
    estimatedEmissionCountsTransformer: CondCountsTransformer[Tag, Sym],
    startEndSymbol: Sym,
    startEndTag: Tag,
    maxIterations: Int = 50,
    minAvgLogProbChangeForEM: Double = 0.00001)
    extends TaggerTrainCommand[Sym, Tag] {

    override def train(
      tagDict: Map[Sym, Set[Tag]],
      rawTrainSequences: Iterable[IndexedSeq[Sym]],
      taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]): Tagger[Sym, Tag] = {

      val trainer =
        new SemisupervisedHmmTaggerTrainer(
          initialTransitionCountsTransformer,
          initialEmissionCountsTransformer,
          estimatedTransitionCountsTransformer,
          estimatedEmissionCountsTransformer,
          startEndSymbol,
          startEndTag,
          maxIterations,
          minAvgLogProbChangeForEM)
      trainer.trainSemisupervised(tagDict, rawTrainSequences, taggedTrainSequences)
    }
  }

  case class SupervisedTaggerTrainCommand[Sym, Tag](
    transitionCountsTransformer: CondCountsTransformer[Tag, Tag],
    emissionCountsTransformer: CondCountsTransformer[Tag, Sym],
    startEndSymbol: Sym,
    startEndTag: Tag)
    extends TaggerTrainCommand[Sym, Tag] {

    override def train(
      tagDict: Map[Sym, Set[Tag]],
      rawTrainSequences: Iterable[IndexedSeq[Sym]],
      taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]): Tagger[Sym, Tag] = {

      val trainer =
        new SupervisedHmmTaggerTrainer(
          transitionCountsTransformer,
          emissionCountsTransformer,
          startEndSymbol,
          startEndTag)
      trainer.trainSupervised(taggedTrainSequences, tagDict)
    }
  }

}

trait TaggerTrainCommandFactory[Sym, Tag] {
  def apply(
    grammar: Option[Map[Tag, Set[Tag]]],
    tagDict: Map[Sym, Set[Tag]],
    rawTrainSequences: Iterable[IndexedSeq[Sym]]): TaggerTrainCommand[Sym, Tag]
}

