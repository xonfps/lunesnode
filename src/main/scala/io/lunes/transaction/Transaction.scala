package io.lunes.transaction

import io.lunes.state2._
import monix.eval.Coeval
import scorex.serialization.{BytesSerializable, JsonSerializable}
import io.lunes.transaction.TransactionParser.TransactionType

/** Base Trait for Lunes Transactions.*/
trait Transaction extends BytesSerializable with JsonSerializable {
  val id: Coeval[ByteStr]

  val transactionType: TransactionType.Value
  val assetFee: (Option[AssetId], Long)
  val timestamp: Long

  override def toString: String = json().toString()

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id() == tx.id()
    case _ => false
  }

  override def hashCode(): Int = id().hashCode()
}

/** Companion Object. */
object Transaction {

  /** Implicit class for Extensions to Lunes Transactions.
     * @param tx The input Transaction.
    */
  implicit class TransactionExt(tx: Transaction) {
    def feeDiff(): Portfolio = tx.assetFee match {
      case (Some(asset), fee) =>
        Portfolio(
          balance = 0,
          leaseInfo = LeaseInfo.empty,
          assets = Map(asset -> fee))
      case (None, fee) => Portfolio(
        balance = fee,
        leaseInfo = LeaseInfo.empty,
        assets = Map.empty)
    }
  }

}
