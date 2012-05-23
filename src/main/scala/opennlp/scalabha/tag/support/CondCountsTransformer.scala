package opennlp.scalabha.tag.support

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Pattern
import org.apache.commons.logging.LogFactory
import util.Random
import collection.{ Map => CMap }

/**
 * A builder for conditional frequency distributions.  Stores counts (in a mutable
 * fashion) and allows counts to be added.  A distribution based on the
 * counts is generated by calling 'toFreqDist'.
 *
 * This is the top of a hierarchy designed for a modular approach to
 * frequency distribution building.  SimpleCondCountsTransformer serves as the basic
 * form of the counter; it stores and increments the actual counts.  Other
 * implementations of CondCountsTransformer will be count-transforming decorators
 * extending DelegatingCondCountsTransformer that wrap SimpleCondCountsTransformer or wrap
 * wrappers thereof.  Multiple layers of decoration allow various
 * transformations to be applied to the counts, and in varying orders.
 *
 * The operation of the system is such that counts, when added via the
 * top-most layer are passed, untouched, all the way to the base where they
 * are stored.  When toFreqDist is called, the counts are gathered via
 * recursive calls to resultCounts that travel down the layers to the bottom,
 * where the true counts are retrieved.  Each layer, starting from the bottom,
 * then applies its transformation and returns the modified counts to be
 * received by the higher layers.  Once the (modified) counts reach the top,
 * they are used to calculate the distribution.
 *
 * For example, the following code will create a CondCountsTransformer that, before
 * creating a distribution, will constrain its counts to those in validEntries
 * and then smooth the constrained counts:
 * {{{
 *   new SimpleSmoothingCondCountsTransformer(lambda,
 *     new ConstrainingCondCountsTransformer(validEntries, strict,
 *       new SimpleCondCountsTransformer()))
 * }}}
 *
 * Implementing classes should define:
 * <ul>
 *   <li> increment: Add to counts. Should simply forward to delegate.
 *   <li> resultCounts: Apply transformation to delegate's resultCounts.
 * </ul>
 *
 * @tparam A	the conditioning item being counted; P(B|A).
 * @tparam B	the conditioned item being counted; P(B|A).
 */
trait CondCountsTransformer[A, B] {
  final def apply[N: Numeric](counts: CMap[A, CMap[B, N]]): DefaultedCondFreqCounts[A, B, Double] =
    this(DefaultedCondFreqCounts(counts.mapValuesStrict(_.mapValuesStrict(implicitly[Numeric[N]].toDouble).toMap).toMap))

  def apply(counts: DefaultedCondFreqCounts[A, B, Double]): DefaultedCondFreqCounts[A, B, Double]
}

//////////////////////////////////////
// Passthrough implementation
//////////////////////////////////////

/**
 * CondCountsTransformer that performs no transformation
 */
case class PassthroughCondCountsTransformer[A, B]() extends CondCountsTransformer[A, B] {
  override def apply(counts: DefaultedCondFreqCounts[A, B, Double]) = counts
}

//////////////////////////////////////
// Constraining Implementation
//////////////////////////////////////

/**
 * CondCountsTransformer decorator that zero out counts for entries not found in
 * validEntries.
 *
 * @param validEntries	zero out entries not found in this set
 * @param delegate		the delegate counter upon which the transformation is performed
 */
case class ConstrainingCondCountsTransformer[A, B](validEntries: Map[A, Set[B]], delegate: CondCountsTransformer[A, B]) extends CondCountsTransformer[A, B] {
  override def apply(counts: DefaultedCondFreqCounts[A, B, Double]) = {
    val resultCounts = delegate(counts)
    val zeroCountAs = DefaultedCondFreqCounts(validEntries.mapValuesStrict(_ => Map[B, Double]())) // a count for every A in validEntries
    //val allCountAs = resultCounts ++ zeroCountAs
    val allBs = (validEntries.values.flatten ++ resultCounts.counts.values.flatMap(_.counts.keySet)).toSet
    val zeroCountBs = FreqCounts(allBs.map(_ -> 0.).toMap)
    DefaultedCondFreqCounts(
      delegate(counts).counts.map {
        case (a, DefaultedFreqCounts(aCounts, aTotalAddition, aDefaultCount)) =>
          validEntries.get(a) match {
            case Some(validBs) =>
              val filtered = FreqCounts(aCounts.filterKeys(validBs)) ++ zeroCountBs
              val defaults = FreqCounts((validBs -- aCounts.keySet).mapTo(b => aDefaultCount).toMap)
              a -> (filtered ++ defaults ++ zeroCountBs).toMap
            case None =>
              a -> Map[B, Double]()
          }
      }) ++ zeroCountAs
  }
}

object ConstrainingCondCountsTransformer {
  def apply[A, B](validEntries: Map[A, Set[B]]): CondCountsTransformer[A, B] =
    ConstrainingCondCountsTransformer(validEntries, PassthroughCondCountsTransformer[A, B]())

  def apply[A, B](validEntriesOpt: Option[Map[A, Set[B]]], delegate: CondCountsTransformer[A, B]): CondCountsTransformer[A, B] =
    validEntriesOpt match {
      case Some(validEntries) => new ConstrainingCondCountsTransformer(validEntries, delegate)
      case None => delegate
    }

  def apply[A, B](validEntries: Option[Map[A, Set[B]]]): CondCountsTransformer[A, B] =
    ConstrainingCondCountsTransformer(validEntries, PassthroughCondCountsTransformer[A, B]())
}

//////////////////////////////////////
// Add-lambda smoothing implementation
//////////////////////////////////////

/**
 * Basic add-lambda smoothing.  A value 'lambda' is added to each count and
 * to the total count.  The 'lambda' value is also used as the default count
 * for any unseen words.
 *
 * @param lambda	smoothing parameter for add-lambda smoothing
 */
case class AddLambdaSmoothingCondCountsTransformer[A, B](lambda: Double, delegate: CondCountsTransformer[A, B]) extends CondCountsTransformer[A, B] {
  override def apply(counts: DefaultedCondFreqCounts[A, B, Double]) = {
    val resultCounts = delegate(counts).counts

    val allBs = resultCounts.flatMap(_._2.counts.keySet).toSet // collect all Bs across all As

    new DefaultedCondFreqCounts(
      resultCounts.mapValuesStrict {
        case DefaultedFreqCounts(c, t, d) =>
          val defaultCounts = FreqCounts((allBs -- c.keySet).mapTo(_ => d).toMap)
          DefaultedFreqCounts((FreqCounts(c) ++ defaultCounts).toMap.mapValuesStrict(_ + lambda), t + lambda, d + lambda)
      })
  }
}

object AddLambdaSmoothingCondCountsTransformer {
  def apply[A, B](lambda: Double): AddLambdaSmoothingCondCountsTransformer[A, B] =
    AddLambdaSmoothingCondCountsTransformer(lambda, PassthroughCondCountsTransformer())
}

//////////////////////////////////////
// Eisner-Smoothing Implementation
//////////////////////////////////////

/**
 * CondCountsTransformer decorator that smoothes counts using the number of
 * single-count items to affect how much smoothing occurs; more single-count
 * items means higher likelihood of out-of-vocabulary items, and thus, more
 * smoothing should be allowed.
 *
 * This is taken from Jason Eisner's HMM homework.
 *
 * @param lambda					smoothing parameter for add-lambda smoothing
 * @param backoffCountsTransformer	used to compute the backoff probability
 */
class EisnerSmoothingCondCountsTransformer[A, B](lambda: Double, backoffCountsTransformer: CountsTransformer[B], delegate: CondCountsTransformer[A, B]) extends CondCountsTransformer[A, B] {
  private val LOG = LogFactory.getLog(classOf[EisnerSmoothingCondCountsTransformer[A, B]])

  override def apply(counts: DefaultedCondFreqCounts[A, B, Double]) = {
    val resultCounts = delegate(counts).counts

    // Compute backoff: probability of B regardless of A
    val totalBackoffCounts = resultCounts.values.flatMap(c => c.simpleCounts).groupByKey.mapValuesStrict(_.sum)
    val transformedBackoffCounts = backoffCountsTransformer(totalBackoffCounts)
    val DefaultedFreqCounts(backoffCounts, backoffTotalAddition, backoffDefaultCount) = transformedBackoffCounts
    val backoffTotal = backoffCounts.values.sum + backoffTotalAddition
    val backoffDist = backoffCounts.mapValuesStrict(_ / backoffTotal)
    val backoffDefault = backoffDefaultCount / backoffTotal

    val allBs = resultCounts.flatMap(_._2.counts.keySet).toSet // collect all Bs across all As

    new DefaultedCondFreqCounts(
      resultCounts.map {
        case (a, DefaultedFreqCounts(aCounts, aTotalAdd, aDefault)) =>
          // Replace any missing counts with the default
          val defaultCounts = FreqCounts((allBs -- aCounts.keySet).mapTo(_ => aDefault).toMap)
          val countsWithDefaults = (FreqCounts(aCounts) ++ defaultCounts).toMap
          
          val numSingleCountItems = countsWithDefaults.count(_._2 < 2.0)
          val smoothedLambda = lambda * (1e-100 + numSingleCountItems)
          val smoothedBackoff = FreqCounts(backoffDist.mapValuesStrict(_ * smoothedLambda))
          val smoothedBackoffDefault = backoffDefault * smoothedLambda
          val smoothedCounts = FreqCounts(countsWithDefaults) ++ smoothedBackoff
          val smoothedDefaultCount = aDefault + smoothedBackoffDefault
          val smoothedTotalAddition = aTotalAdd + smoothedBackoffDefault

          if (LOG.isDebugEnabled && Set("NN", "DT", "N", "D").contains(a.asInstanceOf[String])) {
            LOG.debug(a + ":")
            LOG.debug("    aCounts = " + countsWithDefaults.asInstanceOf[Map[String, Double]].toList.sorted.takeRight(10).map { case (k, v) => "%s -> %.2f".format(k, v) })
            LOG.debug("    smoothedLambda = " + smoothedLambda)
            LOG.debug("    smoothedBackoff = " + smoothedBackoff.toMap.asInstanceOf[Map[String, Double]].toList.sorted.takeRight(10).map { case (k, v) => "%s -> %.2f".format(k, v) })
            LOG.debug("    smoothedCounts  = " + smoothedCounts.toMap.asInstanceOf[Map[String, Double]].toList.sorted.takeRight(10).map { case (k, v) => "%s -> %.2f".format(k, v) })
            LOG.debug("    smoothedDefaultCount = " + smoothedDefaultCount)

            LOG.debug("")

            if (backoffDist contains "the".asInstanceOf[B]) {
              for (w <- List("the", "company").map(_.asInstanceOf[B])) {
                LOG.debug("    c(%s,%s) + sing(%s) * p_back(%s) = %.2f + %.2f * %.2f = %.2f"
                  .format(
                    a, w, a, w,
                    countsWithDefaults.getOrElse(w, 0.), smoothedLambda, backoffDist(w),
                    smoothedCounts.toMap(w)))
              }
              LOG.debug("    smoothedDefaultCount = " + smoothedDefaultCount)
              LOG.debug("")
            }
          }

          (a, DefaultedFreqCounts(smoothedCounts.toMap, smoothedTotalAddition, smoothedDefaultCount))
      })
  }
}

object EisnerSmoothingCondCountsTransformer {
  def apply[A, B](lambda: Double, backoffCountsTransformer: CountsTransformer[B]): EisnerSmoothingCondCountsTransformer[A, B] =
    new EisnerSmoothingCondCountsTransformer(lambda, backoffCountsTransformer, PassthroughCondCountsTransformer[A, B]())
}

//////////////////////////////////////
// Randomizing Implementation
//////////////////////////////////////

/**
 * This transformer adds a (possibly different) random number
 * between 1 and maxCount to each count returned by the delegate.
 */
case class RandomCondCountsTransformer[A, B](maxCount: Int, delegate: CondCountsTransformer[A, B]) extends CondCountsTransformer[A, B] {
  private val rand = new Random(0) // static seed ensures results are reproducible

  override def apply(counts: DefaultedCondFreqCounts[A, B, Double]) = {
    val resultCounts = delegate(counts).counts

    val allBs = resultCounts.flatMap(_._2.counts.keySet).toSet // collect all Bs across all As

    new DefaultedCondFreqCounts(
      resultCounts.mapValuesStrict {
        case DefaultedFreqCounts(c, t, d) =>
          val defaultCounts = FreqCounts((allBs -- c.keySet).mapTo(_ => d).toMap)
          val scaled = (FreqCounts(c) ++ defaultCounts).toMap.mapValuesStrict(_ + rand.nextInt(maxCount + 1))
          DefaultedFreqCounts(scaled, t, d)
      })
  }
}

object RandomCondCountsTransformer {
  def apply[A, B](maxCount: Int): RandomCondCountsTransformer[A, B] =
    RandomCondCountsTransformer(maxCount, PassthroughCondCountsTransformer())
}
