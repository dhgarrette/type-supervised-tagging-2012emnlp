package dhgarrette.typesupervisedtagging.run

import scala.util.Random

import dhgarrette.typesupervisedtagging.minmodel.CoverageMaximizingTagBigramSelector
import dhgarrette.typesupervisedtagging.minmodel.ModelMinimizer
import dhgarrette.typesupervisedtagging.minmodel.NewTagMinimizingTagBigramSelector
import dhgarrette.typesupervisedtagging.minmodel.NewWordTagPairMinimizingTagBigramSelector
import dhgarrette.typesupervisedtagging.minmodel.PassthroughTagBigramSelector
import dhgarrette.typesupervisedtagging.run.MinGreedy.TaggerTrainCommand
import dhgarrette.typesupervisedtagging.run.MinGreedy.SemisupervisedTaggerTrainCommand
import dhgarrette.typesupervisedtagging.run.MinGreedy.SupervisedTaggerTrainCommand
import dhgarrette.typesupervisedtagging.run.MinGreedy.UnsupervisedTaggerTrainCommand
import opennlp.scalabha.tag.hmm.support.StartEndFixingEmissionCountsTransformer
import opennlp.scalabha.tag.hmm.support.EstimatedRawCountUnsupervisedEmissionDistFactory
import opennlp.scalabha.tag.support.AddLambdaSmoothingCondCountsTransformer
import opennlp.scalabha.tag.support.AddLambdaSmoothingCountsTransformer
import opennlp.scalabha.tag.support.CondFreqDist
import opennlp.scalabha.tag.support.ConstrainingCondCountsTransformer
import opennlp.scalabha.tag.support.EisnerSmoothingCondCountsTransformer
import opennlp.scalabha.tag.support.PassthroughCondCountsTransformer
import opennlp.scalabha.tag.support.SimpleTagDictFactory
import opennlp.scalabha.tag.Tagger
import opennlp.scalabha.tag.TaggerEvaluator
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum

object PosExperiments {

  val maxIterations = 50
  val minAvgLogProbChangeForEM = 0.0000001

  def apply(train: String, raw: String, test: String, fullTagset: Set[String]): Unit = {
    val tagDictFile = TaggedFile(train).toList
    val rawTrainLABELED = TaggedFile(raw).takeSub(48000).toList
    val rawTrainFile = rawTrainLABELED.map(_.map(_._1))
    val labeledTest = TaggedFile(test).toList
    val rawTestFile = labeledTest.map(_.map(_._1))

    println("Raw data info:")
    println("    %d word test corpus".format(rawTrainLABELED.flatten.size))
    println("    %d tag bigrams (observed grammar size)".format(rawTrainLABELED.map(s => ("<END>" +: s.map(_._2) :+ "<END>").sliding2).flatten.toSet.groupByKey.flattenOver.size))

    println("Test data info:")
    println("    %d word test corpus".format(labeledTest.flatten.size))
    println("    %d tag bigrams (observed grammar size)".format(labeledTest.map(s => ("<END>" +: s.map(_._2) :+ "<END>").sliding2).flatten.toSet.groupByKey.flattenOver.size))

    val evaluator = (tagger: Tagger[String, String], fullTagDict: Map[String, Set[String]]) => new TaggerEvaluator[String, String]().evaluate(tagger.tag(rawTestFile), labeledTest, fullTagDict)
    val minGreedy = new MinGreedy(evaluator)

    val tagDictFactory = new SimpleTagDictFactory[String, String]()
    val fullTagDict = tagDictFactory.make(tagDictFile)

    println("Tag Dict info:")
    println("    constructed from %d word corpus".format(tagDictFile.flatten.size))
    println("    %d words".format(fullTagDict.size))
    println("    %d word/tag entries".format(fullTagDict.flattenOver.size))
    println("    %.2f per-type ambiguity (td-only)".format(fullTagDict.values.map(_.size).avg))
    println("    %.2f per-token ambiguity (in raw, including using all tags for unknown words)".format(tagDictFile.flatten.map(wt => fullTagDict(wt._1).size).avg))

    {
      val startEndTag = "<END>"
      def end(seq: IndexedSeq[String]) = startEndTag +: seq :+ startEndTag
      def makeGrammar(tagged: Iterable[IndexedSeq[(String, String)]]) = tagged.map(s => end(s.map(_._2)).sliding2).flatten.toSet.groupByKey

      println("\n" +
        "//////////////////////////////////////////////////\n" +
        "  0. Random Baseline" +
        "\n//////////////////////////////////////////////////\n")
      val randomTagger =
        new Tagger[String, String] {
          val rand = new Random
          val allTags = fullTagDict.values.flatten.toSet.toIndexedSeq
          val tagdict = fullTagDict.mapValuesStrict(_.toIndexedSeq).withDefaultValue(allTags)
          def tagSequence(sequence: IndexedSeq[String]): List[String] = {
            sequence.map { w =>
              val tags = tagdict(w)
              tags(rand.nextInt(tags.size))
            }.toList
          }
        }
      evaluator(randomTagger, fullTagDict).toString.split("\n").map("      " + _).foreach(println)

      println("\n" +
        "//////////////////////////////////////////////////\n" +
        "  1. HMM Baseline" +
        "\n//////////////////////////////////////////////////\n")
      val baselineTagger = UniformEmissionUnsupervised(fullTagset)(None, fullTagDict, rawTrainFile).train(fullTagDict, rawTrainFile, null)
      evaluator(baselineTagger, fullTagDict).toString.split("\n").map("      " + _).foreach(println)

      println("\n" +
        "//////////////////////////////////////////////////\n" +
        "  2. HMM Baseline + auto-supervised training" +
        "\n//////////////////////////////////////////////////\n")
      val baselineAutoTagged = baselineTagger.tag(rawTrainFile)
      val autoSupervisedBaselineTagger = Supervised()(None, fullTagDict, null).train(fullTagDict, null, baselineAutoTagged)
      evaluator(autoSupervisedBaselineTagger, fullTagDict).toString.split("\n").map("      " + _).foreach(println)

      println("\n" +
        "//////////////////////////////////////////////////\n" +
        "  3. HMM Baseline + auto-supervised training + emission intialization" +
        "\n//////////////////////////////////////////////////\n")
      val emissionInitializedBaselineTagger = InitializedEmissionUnsupervised()(None, fullTagDict, rawTrainFile).train(fullTagDict, rawTrainFile, null)
      val emissionInitializedBaselineAutoTagged = emissionInitializedBaselineTagger.tag(rawTrainFile)
      val autoSupervisedEmissionInitializedBaselineTagger = Supervised()(None, fullTagDict, null).train(fullTagDict, null, emissionInitializedBaselineAutoTagged)
      evaluator(autoSupervisedEmissionInitializedBaselineTagger, fullTagDict).toString.split("\n").map("      " + _).foreach(println)

      println("\n" +
        "//////////////////////////////////////////////////\n" +
        "  4. MIN-GREEDY with add-one smoothing\n" +
        "  5. MIN-GREEDY with add-one smoothing + auto-supervised" +
        "\n//////////////////////////////////////////////////\n")
      minGreedy.run(
        fullTagDict, rawTrainFile,
        tagDictFactory,
        firstIterationTrainer = UniformEmissionUnsupervised(fullTagset),
        oddIterationTrainer = UniformEmissionUnsupervised(fullTagset),
        evenIterationTrainer = UniformEmissionUnsupervised(fullTagset),
        finalTrainerOpt = Some(Supervised()),
        new ModelMinimizer("<END>",
          tagBigramSelector = new CoverageMaximizingTagBigramSelector(new PassthroughTagBigramSelector),
          getHolesOptimization = false,
          pctSentToComplete = 100),
        pathTagsOnly = false)

      println("\n" +
        "//////////////////////////////////////////////////\n" +
        "  6. MIN-GREEDY with add-one smoothing + auto-supervised + emission init" +
        "\n//////////////////////////////////////////////////\n")
      minGreedy.run(
        fullTagDict, rawTrainFile,
        tagDictFactory,
        firstIterationTrainer = InitializedEmissionUnsupervised(),
        oddIterationTrainer = InitializedEmissionUnsupervised(),
        evenIterationTrainer = InitializedEmissionUnsupervised(),
        finalTrainerOpt = Some(Supervised()),
        new ModelMinimizer("<END>",
          tagBigramSelector = new CoverageMaximizingTagBigramSelector(new PassthroughTagBigramSelector),
          getHolesOptimization = false,
          pctSentToComplete = 100),
        pathTagsOnly = false)

      println("\n" +
        "//////////////////////////////////////////////////\n" +
        "  7. 6 + enhanced tag bigram choice heuristic" +
        "\n//////////////////////////////////////////////////\n")
      minGreedy.run(
        fullTagDict, rawTrainFile,
        tagDictFactory,
        firstIterationTrainer = InitializedEmissionUnsupervised(),
        oddIterationTrainer = InitializedEmissionUnsupervised(),
        evenIterationTrainer = InitializedEmissionUnsupervised(),
        finalTrainerOpt = Some(Supervised()),
        new ModelMinimizer("<END>",
          tagBigramSelector = new NewWordTagPairMinimizingTagBigramSelector(new NewTagMinimizingTagBigramSelector(new CoverageMaximizingTagBigramSelector(new PassthroughTagBigramSelector))),
          getHolesOptimization = false,
          pctSentToComplete = 100),
        pathTagsOnly = false)

      println("\n" +
        "//////////////////////////////////////////////////\n" +
        "  8. 6 + restrict tag bigrams to tag paths of minimization tagged output" +
        "\n//////////////////////////////////////////////////\n")
      minGreedy.run(
        fullTagDict, rawTrainFile,
        tagDictFactory,
        firstIterationTrainer = InitializedEmissionUnsupervised(),
        oddIterationTrainer = InitializedEmissionUnsupervised(),
        evenIterationTrainer = InitializedEmissionUnsupervised(),
        finalTrainerOpt = Some(Supervised()),
        new ModelMinimizer("<END>",
          tagBigramSelector = new CoverageMaximizingTagBigramSelector(new PassthroughTagBigramSelector),
          getHolesOptimization = false,
          pctSentToComplete = 100),
        pathTagsOnly = true)

      println("\n" +
        "//////////////////////////////////////////////////\n" +
        "  9. 6 + transition initialization from minimization tagged output" +
        "\n//////////////////////////////////////////////////\n")
      minGreedy.run(
        fullTagDict, rawTrainFile,
        tagDictFactory,
        firstIterationTrainer = Semisupervised(),
        oddIterationTrainer = InitializedEmissionUnsupervised(),
        evenIterationTrainer = InitializedEmissionUnsupervised(),
        finalTrainerOpt = Some(Supervised()),
        new ModelMinimizer("<END>",
          tagBigramSelector = new CoverageMaximizingTagBigramSelector(new PassthroughTagBigramSelector),
          getHolesOptimization = false,
          pctSentToComplete = 100),
        pathTagsOnly = false)

      println("\n" +
        "//////////////////////////////////////////////////\n" +
        "  10. 6 + 7 + 8 + 9" +
        "\n//////////////////////////////////////////////////\n")
      minGreedy.run(
        fullTagDict, rawTrainFile,
        tagDictFactory,
        firstIterationTrainer = Semisupervised(),
        oddIterationTrainer = InitializedEmissionUnsupervised(),
        evenIterationTrainer = InitializedEmissionUnsupervised(),
        finalTrainerOpt = Some(Supervised()),
        new ModelMinimizer("<END>",
          tagBigramSelector = new NewWordTagPairMinimizingTagBigramSelector(new NewTagMinimizingTagBigramSelector(new CoverageMaximizingTagBigramSelector(new PassthroughTagBigramSelector))),
          getHolesOptimization = false,
          pctSentToComplete = 100),
        pathTagsOnly = true)

    }

  }

  abstract class AbstractUnsupervised[Sym, Tag] extends TaggerTrainCommandFactory[Sym, Tag] {
    def apply(
      grammar: Option[Map[Tag, Set[Tag]]],
      tagDict: Map[Sym, Set[Tag]],
      rawTrainSequences: Iterable[IndexedSeq[Sym]]): TaggerTrainCommand[Sym, Tag] = {

      UnsupervisedTaggerTrainCommand[Sym, Tag](
        initialUnsupervisedEmissionDist = getInitialUnsupervisedEmissionDist(tagDict, rawTrainSequences),
        estimatedTransitionCountsTransformer = ConstrainingCondCountsTransformer(grammar),
        estimatedEmissionCountsTransformer = PassthroughCondCountsTransformer(),
        "<END>".asInstanceOf[Sym], "<END>".asInstanceOf[Tag],
        maxIterations = maxIterations,
        minAvgLogProbChangeForEM = minAvgLogProbChangeForEM)
    }

    def getInitialUnsupervisedEmissionDist(
      tagDict: Map[Sym, Set[Tag]],
      rawTrainSequences: Iterable[IndexedSeq[Sym]]): Tag => Sym => LogNum
  }

  case class InitializedEmissionUnsupervised[Sym, Tag]() extends AbstractUnsupervised[Sym, Tag] {
    override def getInitialUnsupervisedEmissionDist(
      tagDict: Map[Sym, Set[Tag]],
      rawTrainSequences: Iterable[IndexedSeq[Sym]]) = {

      new EstimatedRawCountUnsupervisedEmissionDistFactory(
        AddLambdaSmoothingCountsTransformer(lambda = 1.0),
        tagDict, rawTrainSequences, "<END>".asInstanceOf[Sym], "<END>".asInstanceOf[Tag]).make()
    }
  }

  case class UniformEmissionUnsupervised[Sym, Tag](fullTagset: Set[String]) extends AbstractUnsupervised[Sym, Tag] {
    override def getInitialUnsupervisedEmissionDist(
      tagDict: Map[Sym, Set[Tag]],
      rawTrainSequences: Iterable[IndexedSeq[Sym]]) = {

      val defaultTagset = fullTagset.map(_.asInstanceOf[Tag])
      val allRawWords = rawTrainSequences.flatten.toSet.mapTo(_ => defaultTagset).toMap // for words not in TD, map to all possible tags
      val completedTagDict = (allRawWords ++ tagDict) // if word in TD, use that entry
      val reversedTagDict = completedTagDict.flattenOver.map(_.swap).groupByKey
      val singleCounts = reversedTagDict.mapValuesStrict(_.mapTo(_ => 1).toMap) // one count for each TD entry
      val countsTransformer =
        new StartEndFixingEmissionCountsTransformer[Tag, Sym]("<END>".asInstanceOf[Sym], "<END>".asInstanceOf[Tag],
          AddLambdaSmoothingCondCountsTransformer[Tag, Sym](lambda = 1.)) // add-one smooth
      CondFreqDist(countsTransformer(singleCounts))
    }
  }

  case class Semisupervised[Sym, Tag]() extends TaggerTrainCommandFactory[Sym, Tag] {
    def apply(
      grammar: Option[Map[Tag, Set[Tag]]],
      tagDict: Map[Sym, Set[Tag]],
      rawTrainSequences: Iterable[IndexedSeq[Sym]]): TaggerTrainCommand[Sym, Tag] = {

      SemisupervisedTaggerTrainCommand[Sym, Tag](
        initialTransitionCountsTransformer = EisnerSmoothingCondCountsTransformer(1., AddLambdaSmoothingCountsTransformer(1.)),
        initialEmissionCountsTransformer =
          StartEndFixingEmissionCountsTransformer[Tag, Sym]("<END>".asInstanceOf[Sym], "<END>".asInstanceOf[Tag],
            new EisnerSmoothingCondCountsTransformer(lambda = 1., AddLambdaSmoothingCountsTransformer(lambda = 1.),
              StartEndFixingEmissionCountsTransformer[Tag, Sym]("<END>".asInstanceOf[Sym], "<END>".asInstanceOf[Tag]))),
        estimatedTransitionCountsTransformer = ConstrainingCondCountsTransformer(grammar),
        estimatedEmissionCountsTransformer = PassthroughCondCountsTransformer(),
        "<END>".asInstanceOf[Sym], "<END>".asInstanceOf[Tag],
        maxIterations = maxIterations,
        minAvgLogProbChangeForEM = minAvgLogProbChangeForEM)
    }
  }

  case class Supervised[Sym, Tag]() extends TaggerTrainCommandFactory[Sym, Tag] {
    def apply(
      grammar: Option[Map[Tag, Set[Tag]]],
      tagDict: Map[Sym, Set[Tag]],
      rawTrainSequences: Iterable[IndexedSeq[Sym]]): TaggerTrainCommand[Sym, Tag] = {

      SupervisedTaggerTrainCommand[Sym, Tag](
        transitionCountsTransformer = EisnerSmoothingCondCountsTransformer(1., AddLambdaSmoothingCountsTransformer(1.)),
        emissionCountsTransformer =
          StartEndFixingEmissionCountsTransformer[Tag, Sym]("<END>".asInstanceOf[Sym], "<END>".asInstanceOf[Tag],
            new EisnerSmoothingCondCountsTransformer(lambda = 1., AddLambdaSmoothingCountsTransformer(lambda = 1.),
              StartEndFixingEmissionCountsTransformer[Tag, Sym]("<END>".asInstanceOf[Sym], "<END>".asInstanceOf[Tag]))),
        "<END>".asInstanceOf[Sym], "<END>".asInstanceOf[Tag])
    }
  }

  case class TaggedFile(filename: String) extends Iterable[IndexedSeq[(String, String)]] {
    val WordTagRe = """^(.+)\|([^|]+)$""".r
    def iterator =
      io.Source.fromFile(filename).getLines
        .map(_.trim
          .split("\\s+")
          .map { case WordTagRe(word, tag) => (word, tag) }
          .toIndexedSeq)
  }

  case class RawFile(filename: String) extends Iterable[IndexedSeq[String]] {
    def iterator = io.Source.fromFile(filename).getLines.map(_.trim.split(" ").toIndexedSeq)
  }

  case class AsRawFile(filename: String) extends Iterable[IndexedSeq[String]] {
    def iterator =
      TaggedFile(filename).iterator.map(_.map(_._1))
  }

}
