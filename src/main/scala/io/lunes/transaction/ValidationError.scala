package io.lunes.transaction

import com.google.common.base.Throwables
import io.lunes.state2.ByteStr
import scorex.account.{Address, Alias}
import scorex.block.{Block, MicroBlock}
import io.lunes.transaction.assets.exchange.Order

import scala.util.Either

/** Trait for typing a Validation Error. */
trait ValidationError

/** Companion Object for Validation Error. */
object ValidationError {
  /** Parametrized Type for Checking Validation Errors.
    *  @tparam T Type for T or a Validation Error.
    */
  type Validation[T] = Either[ValidationError, T]

  /** Invalid Address Case Class.
    * @param reason Creates a reason for the invalid Address Error.
    */
  case class InvalidAddress(reason: String) extends ValidationError

  /** Negative Amount Case Class.
    * @param amount Sets the Amount.
    * @param of The error information String.
    */
  case class NegativeAmount(amount: Long, of: String) extends ValidationError

  /** Insuficient Fee Validation Error object.
    */
  case object InsufficientFee extends ValidationError

  /** Array to Big Validation Error object.
    */
  case object TooBigArray extends ValidationError

  /** Ivalid Name Validation Error object.
    */
  case object InvalidName extends ValidationError

  /** Overflow Error Validation Error object.
    */
  case object OverflowError extends ValidationError

  /** Points to itself Validation Error object.
    */
  case object ToSelf extends ValidationError

  /** Missing Sender's Private Key Validation Error object.
    */
  case object MissingSenderPrivateKey extends ValidationError

  /** Unsupported Transaction Type Validation Error object.
    */
  case object UnsupportedTransactionType extends ValidationError

  /** Invalid Request Signature  Validation Error object.
    */
  case object InvalidRequestSignature extends ValidationError

  /** Block From Future object Validation Error Case class.
    * @param ts The Timestamp.
    */
  case class BlockFromFuture(ts: Long) extends ValidationError

  /** Invalid Signature Validation Error Object.
    * @param s The signature.
    * @param details Optional [[InvalidSignature]] details. Default is [[None]].
    */
  case class InvalidSignature(s: Signed, details: Option[InvalidSignature] = None) extends ValidationError {
    override def toString: String = s"InvalidSignature(${s.toString + " reason: " + details})"
  }

  /** Transaction not Allowed Validation Error Case Class.
    * @param t The input Transaction.
    */
  case class TransactionNotAllowedByScript(t: Transaction) extends ValidationError {
    override def toString: String = s"TransactionNotAllowedByScript($t)"
  }

  /** Script Parse Error Validation Error Case Class.
    * @param m Error details.
    */
  case class ScriptParseError(m: String) extends ValidationError

  /** Generic Error Validation Error Case Class.
    * @param err Error Details.
    */
  case class GenericError(err: String) extends ValidationError

  /** Generic Error Companion Object. */
  object GenericError {
    /** Factory Method for GenericError.
      * @param ex Exception.
      * @return Returns a new Generic Error from exception Throw Stack.
      */
    def apply(ex: Throwable): GenericError = new GenericError(Throwables.getStackTraceAsString(ex))
  }

  /** Already in State Validation Error Case Class.
    * @param txId Transaction ID.
    * @param txHeight Transaction Height.
    */
  case class AlreadyInTheState(txId: ByteStr, txHeight: Int) extends ValidationError

  /** Account Balance Error Case Class.
    * @param errs A Map for Account [[Address]] into Error details Strings.
    */
  case class AccountBalanceError(errs: Map[Address, String]) extends ValidationError

  /** Alias Not Exists Validation Error.
    * @param a The Alias.
    */
  case class AliasNotExists(a: Alias) extends ValidationError

  /** Order Validation Error Case Class.
    * @param order The Order.
    * @param err The Error details.
    */
  case class OrderValidationError(order: Order, err: String) extends ValidationError

  /** Sender is Blacklisted Validation Error.
    * @param addr Sender Address.
    */
  case class SenderIsBlacklisted(addr: String) extends ValidationError

  /** Mistiming Validation Error Case Class.
    * @param err Error details.
    */
  case class Mistiming(err: String) extends ValidationError

  /** Block Append Error Validation Error Case Class
    * @param err Error Details.
    * @param b The Unappended Block.
    */
  case class BlockAppendError(err: String, b: Block) extends ValidationError

  /** MicroBlock Append Error Validation Error Case Class.
    * @param err Error Details.
    * @param microBlock The Unappended Microblock.
    */
  case class MicroBlockAppendError(err: String, microBlock: MicroBlock) extends ValidationError {
    override def toString: String = s"MicroBlockAppendError($err, ${microBlock.totalResBlockSig} ~> ${microBlock.prevResBlockSig.trim}])"
  }

  /** Activation Error Validation Error Case Class.
    * @param err Error Details.
    */
  case class ActivationError(err: String) extends ValidationError

}
