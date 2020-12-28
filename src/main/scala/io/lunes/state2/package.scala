package io.lunes

import cats.Monoid
import cats.data.{NonEmptyList => NEL}
import io.lunes.state2.reader.SnapshotStateReader
import monix.eval.Coeval
import io.lunes.transaction.ValidationError.GenericError
import io.lunes.transaction.{Transaction, ValidationError}

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.{Left, Right, Try}

/** Package object for the [[io.lunes.state2]] package. */
package object state2 {

  /** State Reader Type. */
  type StateReader = Coeval[SnapshotStateReader]

  /** Gets a Safe Sum for two Long arguments or returns a Minimum value.
    * @param x First argument.
    * @param y Second argument.
    * @return Returns the safe sum result.
    */
  def safeSum(x: Long, y: Long): Long = Try(Math.addExact(x, y)).getOrElse(Long.MinValue)

  /** An Implicit Class for Extend Either for accept only ValidationError descendants on Left.
    * @param ei The Either type input.
    * @tparam L The Parametrized Type for Left which must be a descendant of [[ValidationError]].
    * @tparam R The Parametrized type for Right.
    */
  implicit class EitherExt[L <: ValidationError, R](ei: Either[L, R]) {
    def liftValidationError[T <: Transaction](t: T): Either[ValidationError, R] = {
      ei.left.map(e => GenericError(e.toString))
    }
  }

  /** An Implicit Class for a second Extension for Either.
    * @param ei The Either type input.
    * @tparam A The Parametrized Type A.
    * @tparam B The Parametrized Type B.
    */
  implicit class EitherExt2[A, B](ei: Either[A, B]) {
    def explicitGet(): B = ei match {
      case Left(value) => throw new Exception(value.toString)
      case Right(value) => value
    }
  }

  /** Gets a integer range.
    * @param from The starting integer.
    * @param to The limiting integer.
    * @param by The integer step.
    * @return Returns a Stream of a Tuple (Int, Int).
    */
  //TODO: Transform in tailrec
  def ranges(from: Int, to: Int, by: Int): Stream[(Int, Int)] =
    if (from + by < to)
      (from, from + by) #:: ranges(from + by, to, by)
    else
      (from, to) #:: Stream.empty[(Int, Int)]

  /** Drop list under condition.
    * @param list The input List.
    * @param cond The anonymous function to boolean.
    * @tparam A The Parametrized Type A.
    * @return Returns the Dropped list.
    */
  @tailrec // added.
  def dropLeftIf[A](list: List[A])(cond: List[A] => Boolean): List[A] = list match {
    case l@(x :: xs) => if (cond(l)) dropLeftIf(xs)(cond) else l
    case Nil => Nil
  }

  /** Split list after a given Threshold.
    * @param list A Non Empty List.
    * @param threshold The Integer Threshold.
    * @param count A count anonymous function.
    * @tparam A Type Parameter.
    * @return Returns a Tuple for Non Empty List and a List for Type parameter A.
    */
  def splitAfterThreshold[A](list: NEL[A], threshold: Int)(count: A => Int): (NEL[A], List[A]) = {
    @tailrec
    def splitR(agg: NEL[A], aggCount: Int, rest: List[A]): (NEL[A], List[A]) =
      if (aggCount >= threshold) (agg, rest)
      else rest match {
        case Nil => (agg, rest)
        case (x :: xs) => splitR(x :: agg, count(x) + aggCount, xs)
      }

    val r = splitR(NEL.one(list.head), count(list.head), list.tail)
    (r._1.reverse, r._2)
  }

  /** Generates a Non Empty List for Prepended Compact.
    * @param `new` A new element.
    * @param existing Existin list.
    * @param compactPred Compact Pred.
    * @param ma A Monoid of A
    * @tparam A Type PArameter of A
    * @return Returns a Non Emnpty List of A.
    */
  def prependCompact[A](`new`: A, existing: NEL[A])(compactPred: (A, A) => Boolean)(implicit ma: Monoid[A]): NEL[A] = {
    if (compactPred(`new`, existing.head)) NEL(Monoid.combine(existing.head, `new`), existing.tail)
    else `new` :: existing
  }

  /** Generates a Prepend for Compact Block Diff.
    * @param `new` The new BlockDiff.
    * @param existing The Existing List.
    * @param maxTxsInChunk Maximum Number of Transactions per Chunck.
    * @return Returns the Non Empty List.
    */
  def prependCompactBlockDiff(`new`: BlockDiff, existing: NEL[BlockDiff], maxTxsInChunk: Int): NEL[BlockDiff] =
    prependCompact[BlockDiff](`new`, existing) { case (x, y) => x.txsDiff.transactions.size + y.txsDiff.transactions.size <= maxTxsInChunk }

  /** Check to see if both entries have the same quotient.
    * @param x First Input.
    * @param y Second Input.
    * @param divisor Divisor.
    * @return Returns true ir the quotients are the same.
    */
  def sameQuotient(x: Int, y: Int, divisor: Int): Boolean = (x / divisor) == (y / divisor)

  /** Defines a Cast implicit class.
    * @param a The input Class.
    * @tparam A The Type Parameter.
    */
  implicit class Cast[A](a: A) {
    /** Forces casting under a Class. It is not fitted for Traits or Objects inputs.
      * @tparam B Type Parameter which must be a class.
      * @return Returns an Optinal object of type B.
      */
    def cast[B: ClassTag]: Option[B] = {
      a match {
        case b: B => Some(b)
        case _ => None
      }
    }
  }

}
