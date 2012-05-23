package dhgarrette.typesupervisedtagging.util

import org.apache.commons.logging.Log

object Time {

  def time[T](name: String, block: => T): T = {
    val startTime = System.currentTimeMillis()
    println("starting: " + name)
    val (r, t) = timer(block)
    println("finished: " + name + " in " + t + " seconds")
    r
  }

  def time[T](name: String, block: => T, LOG: Log): T = {
    val startTime = System.currentTimeMillis()
    LOG.info("starting: " + name)
    val (r, t) = timer(block)
    LOG.info("finished: " + name + " in " + t + " seconds")
    r
  }

  def time1[T](name: String, block: => T): T = {
    val startTime = System.currentTimeMillis()
    val (r, t) = timer(block)
    println(name + " - " + t + " seconds")
    r
  }

  def time1[T](name: String, block: => T, LOG: Log): T = {
    val startTime = System.currentTimeMillis()
    val (r, t) = timer(block)
    LOG.info(name + " - " + t + " seconds")
    r
  }

  def timer[T](block: => T): (T, Double) = {
    val startTime = System.currentTimeMillis()
    val r = block
    (r, (System.currentTimeMillis() - startTime) / 1000.0)
  }

}
