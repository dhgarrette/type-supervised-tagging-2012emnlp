package dhgarrette.typesupervisedtagging.util

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter

import scala.collection.mutable.ListBuffer
import scala.util.Random

object FileUtils {

  private val random = new Random(System.currentTimeMillis())

  def pathjoin(parts: String*): String = {
    (parts.dropRight(1).filter(_.nonEmpty).map(p => if (p.endsWith(File.separator)) p.dropRight(File.separator.length) else p) :+ parts.last).mkString(File.separator)
  }

  def openForWrite(filename: String): BufferedWriter =
    new BufferedWriter(new FileWriter(filename))

  def dumpToFile(content: String, filename: String): String = {
    writeUsing(filename) { w =>
      w.write(content)
    }
  }

  def mktemp(prefix: String = "temp-", suffix: String = ""): String = {
    val f = File.createTempFile(prefix, suffix)
    f.delete()
    return f.getAbsolutePath
//    return "/tmp/" + f.getName()
  }

  def insertTempSubdir(file: String, mkDir: Boolean = false) = {
    val (origPath, hadoopGraphFilename) = getPathAndFile(file)
    val path =
      if (new File(origPath).getName != "temp") {
        val newPath = pathjoin(origPath, "temp")
        val pathObj = new File(newPath)
        if (mkDir && !pathObj.exists)
          pathObj.mkdirs()
        newPath
      } else
        origPath
    pathjoin(path, hadoopGraphFilename)
  }

  def getPathAndFile(pathAndFile: String) = {
    val absFile = new File(pathAndFile).getAbsoluteFile
    (absFile.getParent, absFile.getName)
  }

  def remove(filename: String) = {
    new File(filename).delete()
  }

  implicit def string2file(s: String) = new File(new File(s).getAbsolutePath)
  implicit def file2recursivelyDeletableFile(f: File) = new RecursivelyDeletableFile(f)

  class RecursivelyDeletableFile(private val f: File) {
    def recursiveDelete(): Boolean = {
      if (f.isDirectory)
        f.listFiles.filter(null !=).foreach(_.recursiveDelete())
      return f.delete()
    }
  }

  def exists(filename: String) =
    new File(filename).exists()

  def findBinary(name: String, binDir: Option[String] = None, envar: Option[String] = None, verbose: Boolean = false): String = {
    val checked = new ListBuffer[String]

    if (binDir.isDefined) {
      val path = FileUtils.pathjoin(binDir.get, name)
      if (FileUtils.exists(path))
        return path
      else
        checked += path
    }

    if (envar.isDefined) {
      val envpath = System.getenv(envar.get)
      if (envpath != null) {
        val path = FileUtils.pathjoin(envpath, name)
        if (FileUtils.exists(path))
          return path
        else
          checked += path
      }
    }

    try {
      return scala.sys.process.Process(List("which", name)) !!;
    } catch {
      case _ => {
        checked += "which " + name
      }
    }

    throw new RuntimeException("No binary found.  Checked the following:\n" + checked.map((" ") * 16 + _).mkString("\n"))
  }

  /**
   * Automatic Resource Management
   *
   * using(new BufferedReader(new FileReader("file"))) { r =>
   *   var count = 0
   *   while (r.readLine != null) count += 1
   *   println(count)
   * }
   */
  def using[T <: { def close() }](resource: T)(block: T => Unit) {
    try {
      block(resource)
    } finally {
      if (resource != null) resource.close()
    }
  }

  def writeUsing(filename: String)(block: BufferedWriter => Unit): String = {
    using(openForWrite(filename))(block)
    filename
  }

}
