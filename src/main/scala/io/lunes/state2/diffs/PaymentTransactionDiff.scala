package io.lunes.state2.diffs

import cats.implicits._
import io.lunes.settings.FunctionalitySettings
import io.lunes.state2.reader.SnapshotStateReader
import io.lunes.state2.{Diff, LeaseInfo, Portfolio}
import scorex.account.Address
import io.lunes.transaction.ValidationError.GenericError
import io.lunes.transaction.{PaymentTransaction, ValidationError}

import scala.util.{Left, Right}

/** Payment Transaction Diff Object.*/
object PaymentTransactionDiff {
  /** The Application method for Payment Transaction Diff.
    * @param stateReader The Snapshot State Reader object.
    * @param height The height of the Transaction.
    * @param settings the Functional Settings.
    * @param blockTime The Block's Time.
    * @param tx The PaymentTransaction.
    * @return Returns Either a Diff (case Success) or a ValidationError (case Failure).
    */
  def apply(stateReader: SnapshotStateReader, height: Int, settings: FunctionalitySettings, blockTime: Long)
           (tx: PaymentTransaction): Either[ValidationError, Diff] = {

    if (height > settings.blockVersion3AfterHeight) {
      Left(GenericError(s"Payment transaction is deprecated after h=${settings.blockVersion3AfterHeight}"))
    } else {
      Right(Diff(height = height,
        tx = tx,
        portfolios = Map(
          tx.recipient -> Portfolio(
            balance = tx.amount,
            LeaseInfo.empty,
            assets = Map.empty)) combine Map(
          Address.fromPublicKey(tx.sender.publicKey) -> Portfolio(
            balance = -tx.amount - tx.fee,
            LeaseInfo.empty,
            assets = Map.empty
          )),
      ))
    }
  }
}