package dhgarrette.typesupervisedtagging.data

import java.io.File
import java.io.BufferedReader
import dhgarrette.typesupervisedtagging.util.FileUtils._
import java.io.FileReader
import scala.io.Source
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.Writer
import scala.collection.mutable.Buffer
import dhgarrette.typesupervisedtagging.util.FileUtils

object ExtractPostags {

  val TERMINAL_RE = """\((\S+) (\S+)\)""".r

  class SentenceIterator(private val files: Iterator[String]) extends Iterator[String] {
    private var linesIterator: Iterator[String] = Iterator.empty

    override def next(): String = {
      while (hasNext) {
        val line = linesIterator.next
        if (line.trim.startsWith("(")) {
          return line
        }
      }
      throw new RuntimeException()
    }

    override def hasNext(): Boolean = {
      while (true) {
        if (linesIterator.hasNext)
          return true
        else if (files.hasNext)
          linesIterator = {
            val x = Source.fromFile(files.next).getLines.mkString(" ")
            var pc = 0
            var start = 0
            val sentences = Buffer[String]()
            for ((c, i) <- x.zipWithIndex) {
              if (c == '(') {
                if (pc == 0)
                  start = i
                pc += 1
              }
              if (c == ')') {
                pc -= 1
                if (pc == 0)
                  sentences.append(x.substring(start, i + 1))
              }
            }
            sentences.iterator
          }
        else
          return false
      }
      throw new AssertionError
    }
  }

  def main(args: Array[String]): Unit = {
    val COMB_DIR = FileUtils.pathjoin(args(0), "combined")
    require(new File(COMB_DIR).exists, "Directory '%s' does not exist".format(COMB_DIR))
    val FN_RE = """^wsj_(\d\d)(\d\d)\.mrg$""".r

    //    val TRAIN_OUT = "data/s00-18."
    //    val DEV_OUT = "data/s19-21."
    //    val TEST_OUT = "data/s22-24."

    def readSections(sections: Range) =
      new SentenceIterator((
        for (
          f <- new File(COMB_DIR).listFiles;
          fn = Some(f.getName);
          FN_RE(sec, _) <- fn if sections.contains(sec.toInt)
        ) yield f.getAbsolutePath).sorted.iterator)

    def posFormatter(word: String, pos: String) = "%s|%s".format(word, pos)

    def writeSentences(sec: Range, formatter: (String, String) => String) = {
      writeUsing("data/s%02d-%02d.pos".format(sec.head, sec.last)) { w =>
        for (line <- readSections(sec))
          w.write(
            TERMINAL_RE.findAllIn(line).matchData
              .map(_.subgroups)
              .flatMap {
                case Seq("-NONE-", word) => None
                case Seq(pos, word) => Some(formatter(word, pos))
              }
              .mkString(" ") + "\n")
      }
    }

    writeSentences(0 to 7, posFormatter)
    writeSentences(0 to 15, posFormatter)
    writeSentences(16 to 18, posFormatter)
    writeSentences(19 to 21, posFormatter)
    writeSentences(22 to 24, posFormatter)

  }
}
