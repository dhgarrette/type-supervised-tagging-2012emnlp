package dhgarrette.typesupervisedtagging.run

import org.apache.log4j.Level
import org.apache.log4j.Logger

import dhgarrette.typesupervisedtagging.data.ExtractPostags

object Run {

  val fullTagsetEn = Set("#", "$", "''", ",", "-LRB-", "-RRB-", ".", ":", "CC", "CD", "DT", "EX", "FW", "IN", "JJ", "JJR", "JJS", "LS", "MD", "NN", "NNP", "NNPS", "NNS", "PDT", "POS", "PRP", "PRP$", "RB", "RBR", "RBS", "RP", "SYM", "TO", "UH", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "WDT", "WP", "WP$", "WRB", "``")
  val fullTagsetIt73 = Set("\"", "VMA~IN", "'", "VAU~IM", "ADVB", "ADJ~DE", "VMO~IM", "ADJ~IN", "PRO~ID", "VMA~PA+REDUC", "PHRAS", "ADJ~QU", ":", "VMO~PA", "VMA~CG", "VAU~RE", "NOU~CP", "VMO~GE", "VAU~FU", "PRO~PE", "PUNCT", "ART~DE", ",", "SPECIAL", "VAU~RA", "-RRB-", "VMO~CG", "VMO~RA", "ADJ~DI-SBJ", "VAU~PA", ".", "NOU~CP2", "ADJ~OR", "NOU~CA", "NUMR", "-LRB-", "PRO~LO", "VMO~CO", "PRO~DE", "ADJ~IR-SBJ", "VAU~IN", "NOU~PR", "PRO~RI", "CONJ", "VMA~IP", "VMA~PA", "VAU~GE", "VMA~RE", "VMA~FU", "ADJ~PO", "PRO~RE", "ART~IN", "VMA~PE", "VMO~IN", "PREP", "ADJ~IR", "PRO~PO", "VMA~CO", "ADJ~EX", "VMA~RA", "VMA~IM", "VMO~FU", "VMA~GE", "DATE", "ADJ~DI", "VAU~CG", "VMA~+REDUC", "VAU~CO", "PRO~OR", "VMO~RE", "PRO~IN", "PRDT", "NOU~CS")

  val ptb16Train = "data/s00-15.pos"
  val ptb8Train = "data/s00-07.pos"
  val ptbRaw = "data/s16-18.pos"
  val ptbDev = "data/s19-21.pos"
  val ptbTest = "data/s22-24.pos"

  val tutTrain73 = "data/tut-73tags-train.pos"
  val tutRaw73 = "data/tut-73tags-raw.pos"
  val tutTest73 = "data/tut-73tags-test.pos"

  def main(args: Array[String]) {
    Logger.getRootLogger.setLevel(Level.INFO)
    //Logger.getLogger("opennlp.scalabha.tag.hmm.HmmTaggerTrainer$").setLevel(Level.ERROR)

    args.toList match {
      case Seq("en-data", treebankLocation) => ExtractPostags.main(Array(treebankLocation))
      case Seq("en-run16") => PosExperiments(ptb16Train, ptbRaw, ptbTest, fullTagsetEn)
      case Seq("en-run8") => PosExperiments(ptb8Train, ptbRaw, ptbTest, fullTagsetEn)

      //case Seq("it-data", treebankLocation) => ExtractPostagsTut.main(Array(treebankLocation))
      case Seq("it-run") => PosExperiments(tutTrain73, tutRaw73, tutTest73, fullTagsetIt73)
    }

  }
}
