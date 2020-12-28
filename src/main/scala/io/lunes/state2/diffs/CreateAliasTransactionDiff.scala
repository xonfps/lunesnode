package io.lunes.state2.diffs

import io.lunes.state2.{Diff, LeaseInfo, Portfolio}
import io.lunes.transaction.{CreateAliasTransaction, ValidationError}

import scala.util.Right

/** Create Alias Transaction Difference object.*/
object CreateAliasTransactionDiff {
  /** The Application method for the create alias Transaction difference.
    * @param height The Height of the Block.
    * @param tx The Create Alias Transaction input.
    * @return Returns Either a Diff (case Success) or a ValidationError (case Failure).
    */
  def apply(height: Int)(tx: CreateAliasTransaction): Either[ValidationError, Diff] = {
    Right(Diff(height = height, tx = tx,
      portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
      aliases = Map(tx.alias -> tx.sender.toAddress)
    ))
  }
}
