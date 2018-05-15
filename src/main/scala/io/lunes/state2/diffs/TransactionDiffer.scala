package io.lunes.state2.diffs


import io.lunes.features.FeatureProvider
import io.lunes.settings.FunctionalitySettings
import io.lunes.state2._
import io.lunes.state2.reader.SnapshotStateReader
import io.lunes.transaction.ValidationError.UnsupportedTransactionType
import io.lunes.transaction._
import io.lunes.transaction.assets._
import io.lunes.transaction.assets.exchange.ExchangeTransaction
import io.lunes.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

/** Transaction Differ object.*/
object TransactionDiffer {

  /** Case Class for a Transaction Validation Error.
    * @param cause The Error Cause.
    * @param tx The input Transaction.
    */
  case class TransactionValidationError(cause: ValidationError, tx: Transaction) extends ValidationError

  /** The Application method for the Transaction Differ object.
    * @param settings The Functional Settings.
    * @param prevBlockTimestamp The Previous Block Timestamp.
    * @param currentBlockTimestamp The Current Block Timestamp.
    * @param currentBlockHeight The Current Block Height.
    * @param s The Snapshot State Reader.
    * @param fp The Feature Provider.
    * @param tx The Transaction object.
    * @return Returns Either a Diff (case Success) or a ValidationError (case Failure).
    */
  def apply(settings: FunctionalitySettings, prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int)
           (s: SnapshotStateReader, fp: FeatureProvider, tx: Transaction): Either[ValidationError, Diff] = {
    for {
      t0 <- Verifier(s, currentBlockHeight)(tx)
      t1 <- CommonValidation.disallowTxFromFuture(settings, currentBlockTimestamp, t0)
      t2 <- CommonValidation.disallowTxFromPast(prevBlockTimestamp, t1)
      t3 <- CommonValidation.disallowBeforeActivationTime(fp, currentBlockHeight, t2)
      t4 <- CommonValidation.disallowDuplicateIds(s, settings, currentBlockHeight, t3)
      t5 <- CommonValidation.disallowSendingGreaterThanBalance(s, settings, currentBlockTimestamp, t4)
      diff <- t5 match {
        case gtx: GenesisTransaction => GenesisTransactionDiff(currentBlockHeight)(gtx)
        case ptx: PaymentTransaction => PaymentTransactionDiff(s, currentBlockHeight, settings, currentBlockTimestamp)(ptx)
        case itx: IssueTransaction => AssetTransactionsDiff.issue(currentBlockHeight)(itx)
        case rtx: ReissueTransaction => AssetTransactionsDiff.reissue(s, settings, currentBlockTimestamp, currentBlockHeight)(rtx)
        case btx: BurnTransaction => AssetTransactionsDiff.burn(s, currentBlockHeight)(btx)
        case ttx: TransferTransaction => TransferTransactionDiff(s, settings, currentBlockTimestamp, currentBlockHeight)(ttx)
        case rdtx: RegistryTransaction => RegistryTransactionDiff(s, settings, currentBlockTimestamp, currentBlockHeight)(rdtx)
        case mtx: MassTransferTransaction => MassTransferTransactionDiff(s, currentBlockTimestamp, currentBlockHeight)(mtx)
        case ltx: LeaseTransaction => LeaseTransactionsDiff.lease(s, currentBlockHeight)(ltx)
        case ltx: LeaseCancelTransaction => LeaseTransactionsDiff.leaseCancel(s, settings, currentBlockTimestamp, currentBlockHeight)(ltx)
        case etx: ExchangeTransaction => ExchangeTransactionDiff(s, currentBlockHeight)(etx)
        case atx: CreateAliasTransaction => CreateAliasTransactionDiff(currentBlockHeight)(atx)
        case _ => Left(UnsupportedTransactionType)
      }
      positiveDiff <- BalanceDiffValidation(s, settings)(diff)
    } yield positiveDiff
  }.left.map(TransactionValidationError(_, tx))
}
