package io.lunes.state2.diffs

import cats.implicits._
import io.lunes.state2._
import io.lunes.state2.reader.SnapshotStateReader
import scorex.account.Address
import io.lunes.transaction.ValidationError
import io.lunes.transaction.ValidationError.{GenericError, Validation}
import io.lunes.transaction.assets.MassTransferTransaction
import io.lunes.transaction.assets.MassTransferTransaction.ParsedTransfer

/** Mass Transfer Transaction Difference object. */
object MassTransferTransactionDiff {
  /** The Application method for the Mass Transfer Transaction Difference.
    * @param state The Snapshot State Reader object.
    * @param blockTime The Block's Time.
    * @param height The Height of the Block.
    * @param tx The MassTransferTransaction.
    * @return Returns Either a Diff (case Success) or a ValidationError (case Failure).
    */
  def apply(state: SnapshotStateReader, blockTime: Long, height: Int)(tx: MassTransferTransaction): Either[ValidationError, Diff] = {
    def parseTransfer(xfer: ParsedTransfer): Validation[(Map[Address, Portfolio], Long)] = {
      for {
        recipientAddr <- state.resolveAliasEi(xfer.address)
        portfolio = tx.assetId match {
          case None => Map(recipientAddr -> Portfolio(xfer.amount, LeaseInfo.empty, Map.empty))
          case Some(aid) => Map(recipientAddr -> Portfolio(0, LeaseInfo.empty, Map(aid -> xfer.amount)))
        }
      } yield (portfolio, xfer.amount)
    }
    val portfoliosEi = tx.transfers.traverse(parseTransfer)

    portfoliosEi.flatMap { list: List[(Map[Address, Portfolio], Long)] =>
      val sender = Address.fromPublicKey(tx.sender.publicKey)
      val foldInit = (Map(sender -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)), 0L)
      val (recipientPortfolios, totalAmount) = list.fold(foldInit) { (u, v) => (u._1 combine v._1, u._2 + v._2) }
      val completePortfolio = recipientPortfolios.combine(
        tx.assetId match {
          case None => Map(sender -> Portfolio(-totalAmount, LeaseInfo.empty, Map.empty))
          case Some(aid) => Map(sender -> Portfolio(0, LeaseInfo.empty, Map(aid -> -totalAmount)))
        })

      val assetIssued = tx.assetId match {
        case None => true
        case Some(aid) => state.assetInfo(aid).isDefined
      }
      Either.cond(assetIssued,
        Diff(height, tx, completePortfolio),
        GenericError(s"Attempt to transfer a nonexistent asset"))
    }
  }
}