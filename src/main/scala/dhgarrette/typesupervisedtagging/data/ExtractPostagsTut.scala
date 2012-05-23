package dhgarrette.typesupervisedtagging.data

import java.io.File
import java.io.BufferedReader
import dhgarrette.typesupervisedtagging.util.FileUtils._
import opennlp.scalabha.util.CollectionUtils._
import java.io.FileReader
import scala.io.Source
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.Writer
import scala.collection.mutable.Buffer

/**
 * TUT Split	total  	train 	raw 	test
 * CODICECIVILE	1100	600		250		250
 * NEWS	        700	    400		150		150
 * VEDCH		400		200		100		100
 * EUDIR		200		100		50		50
 * WIKI			459		200		150		109
 * 				2859	1500	700		659
 */

object ExtractPostagsTut {

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
    def read(sec: String, trainNum: Int, rawNum: Int) = {
      val Pos1Re = """^(.+)-[.0-9]+$""".r
      val Pos2Re = """^(.+)~[^~]*$""".r

      def cleanPosFine(p: String) = {
        p match { case Pos1Re(t) => t; case t => t }
      }

      def cleanPosCoarse(p: String) = {
        cleanPosFine(p) match { case Pos2Re(t) => t; case t => t }
      }

      val sentences =
        new SentenceIterator(Iterator(new File("data/tut/%s.penn".format(sec)).getAbsolutePath))
          .map(line =>
            TERMINAL_RE.findAllIn(line).matchData
              .map(_.subgroups)
              .flatMap {
                case Seq("-NONE-", word) => None
                case Seq(pos, word) => Some((word.replaceAll("\\)", ""), cleanPosFine(pos)))
              }
              .map { case (w, t) => if (w.trim.isEmpty) throw new RuntimeException(w); (w, t) }
              .toList)
          .toList

      val (train, rawTest) = sentences.splitAt(trainNum)
      val (raw, test) = rawTest.splitAt(rawNum)

      println(sec)
      println("Num sentences: " + sentences.size)
      println("Num tokens: " + sentences.flatten.size)
      println("train sentences: " + train.size)
      println("raw sentences: " + raw.size)
      println("test sentences: " + test.size)

      (train, raw, test)
    }

    val (trains, raws, tests) =
      List(
        read("codicecivile", 600, 250),
        read("news", 400, 150),
        read("vedch", 200, 100),
        read("eudir", 100, 50),
        read("wiki", 200, 150))
        .unzip3

    val train = trains.flatten
    val raw = raws.flatten
    val test = tests.flatten

    val allTags = (train ++ raw ++ test).flatten.map(_._2).toSet
    println("allTags = " + allTags.map(t => '"' + t.replace("\"", "\\\"") + '"').mkString("Set(", ",", ")"))

    def write(name: String, stuff: Iterable[List[(String, String)]]) = {
      println("%s: %s sentences, %s tokens".format(name, stuff.size, stuff.flatten.size))
      writeUsing("data/tut-%s.pos".format(name)) { w =>
        for (line <- stuff)
          w.write(line.map { case (w, p) => "%s|%s".format(w, p) }.mkString(" ") + "\n")
      }
    }

    write("train", train)
    write("raw", raw)
    write("test", test)

  }
}
