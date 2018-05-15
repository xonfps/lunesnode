package io.lunes.state2.reader

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.data.{NonEmptyList => NEL}
import cats.implicits._
import cats.kernel.Monoid
import io.lunes.state2._
import monix.eval.Coeval
import scorex.account.{Address, Alias}
import io.lunes.transaction.Transaction
import io.lunes.transaction.lease.LeaseTransaction

/** Class for a Compsite Snapshot State Reader. This Class uses a Private constructor.
  * @param inner The Snapshot State Reader.
  * @param blockDiff BlockDiff object to compose.
  */
class CompositeStateReader private(inner: SnapshotStateReader, blockDiff: BlockDiff) extends SnapshotStateReader {
  /** Gets the Synchronization Token.
    * @return Returns the inner synchronization Token.
    */
  def synchronizationToken: ReentrantReadWriteLock = inner.synchronizationToken

  private val txDiff = blockDiff.txsDiff

	/** Gets Transaction Information.
		* @param id Inputs the Transaction ID.
		* @return Returns an Option for a Tuple (Int, Option[Transaction]).
		*/
  override def transactionInfo(id: ByteStr): Option[(Int, Option[Transaction])] =
    txDiff.transactions.get(id)
      .map(t => (t._1, Some(t._2)))
      .orElse(inner.transactionInfo(id))

	/** Gets the Account Portfolio.
		* @param a Inputs an [[scorex.account.Address]].
		* @return Returns the Portfolio object.
		*/
  override def accountPortfolio(a: Address): Portfolio =
    inner.accountPortfolio(a).combine(txDiff.portfolios.get(a).orEmpty)

	/** Gets the Asset Information.
		* @param id Inputs Asset ID.
		* @return Returns an Option for [[io.lunes.state2.AssetInfo]].
		*/
  override def assetInfo(id: ByteStr): Option[AssetInfo] = (inner.assetInfo(id), txDiff.issuedAssets.get(id)) match {
    case (None, None) => None
    case (existing, upd) => Some(existing.orEmpty.combine(upd.orEmpty))
  }

	/** Gets the Total Block Height.
		* @return Returns the height.
		*/
  override def height: Int = inner.height + blockDiff.heightDiff

	/** Gets the Account Transaction IDs.
		* @param a Inputs a [[scorex.account.Address]].
		* @param limit Sets a limit for transaction retrieval.
		* @return Returns a Seguence of IDs for Transactions.
		*/
  override def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr] = {
    val fromDiff = txDiff.accountTransactionIds.get(a).orEmpty
    if (fromDiff.lengthCompare(limit) >= 0) {
      fromDiff.take(limit)
    } else {
      fromDiff ++ inner.accountTransactionIds(a, limit - fromDiff.size) // fresh head ++ stale tail
    }
  }

	/** Gets a Snapshot at a certain Height.
		* @param acc Inputs the [[scorex.account.Address]].
		* @param h Inputs the Height.
		* @return Returns an Option for [[io.lunes.state2.Snapshot]].
		*/
  override def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot] =
    blockDiff.snapshots.get(acc).flatMap(_.get(h)).orElse(inner.snapshotAtHeight(acc, h))

	/** Get all Aliasses of a given Address.
		* @param a Inputs a [[scorex.account.Address]]
		* @return Returns a Sequence of [[scorex.account.Alias]].
		*/
  override def aliasesOfAddress(a: Address): Seq[Alias] =
    txDiff.aliases.filter(_._2 == a).keys.toSeq ++ inner.aliasesOfAddress(a)

	/** Resolves a given Alias.
		* @param a Inputs a [[scorex.account.Alias]].
		* @return Returns an Option for [[scorex.account.Address]].
		*/
  override def resolveAlias(a: Alias): Option[Address] = txDiff.aliases.get(a).orElse(inner.resolveAlias(a))

	/** Get a Map for Accounts into Portfolios.
		* @return Returns a Map for [[scorex.account.Address]] into [[io.lunes.state2.Portfolio]].
		*/
  override def accountPortfolios: Map[Address, Portfolio] = Monoid.combine(inner.accountPortfolios, txDiff.portfolios)

	/** Check if Leaf is Active.
		* @param leaseTx Inpust a [[io.lunes.transaction.lease.LeaseTransaction]].
		* @return Returns true if the Lease is active.
		*/
  override def isLeaseActive(leaseTx: LeaseTransaction): Boolean =
    blockDiff.txsDiff.leaseState.getOrElse(leaseTx.id(), inner.isLeaseActive(leaseTx))

	/** Get a Sequence of Active Leases.
		* @return Returns a Sequence of IDs for Active Leases.
		*/
  override def activeLeases(): Seq[ByteStr] = {
    val newestLeaseState = blockDiff.txsDiff.leaseState
    newestLeaseState.collect { case (id, true) => id }.toSeq ++
      inner.activeLeases().filter(newestLeaseState.getOrElse(_, true))
  }

	/** Gets the Height of the last update.
		* @param acc Inputs an [[scorex.account.Address]].
		* @return Returns an Option for the Height.
		*/
  override def lastUpdateHeight(acc: Address): Option[Int] = blockDiff.snapshots.get(acc).map(_.lastKey).orElse(inner.lastUpdateHeight(acc))

	/** Check if given ID is assigned to some Transaction.
		* @param id Inputs the Transaction ID.
		* @return Return true if exists.
		*/
  override def containsTransaction(id: ByteStr): Boolean = blockDiff.txsDiff.transactions.contains(id) || inner.containsTransaction(id)

	/** Get Order Fill Info for filled Volume and Fees.
		* @param orderId Inpúts the order ID.
		* @return Return an [[io.lunes.state2.OrderFillInfo]].
		*/
  override def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo =
    blockDiff.txsDiff.orderFills.get(orderId).orEmpty.combine(inner.filledVolumeAndFee(orderId))

	/** Get Lunes Balance for a given Account Address.
		* @param a Inputs an [[scorex.account.Address]].
		* @return Returns a Tuple (Long, LeaseInfo).
		*/
  override def lunesBalance(a: Address): (Long, LeaseInfo) = {
    val in = inner.partialPortfolio(a)
    val diffed = blockDiff.txsDiff.portfolios.get(a).orEmpty
    (in.balance + diffed.balance, Monoid.combine(diffed.leaseInfo, in.leaseInfo))
  }

	/** Gets the Asset Balance at a given Account Address.
		* @param a Inputs an [[scorex.account.Address]]
		* @param asset Inputs Asset ID.
		* @return Returns the Asset Balance.
		*/
  override def assetBalance(a: Address, asset: ByteStr): Long = {
    val in: Long = inner.assetBalance(a, asset)
    val diffed: Long = blockDiff.txsDiff.portfolios.get(a).orEmpty.assets.getOrElse(asset, 0)
    in + diffed
  }
}

/** Companion Object fro CompositeStateReader. */
object CompositeStateReader {
	/** Factory method inverting inputs.
		* @param blockDiff The BlockDiff object to compose.
		* @param inner The Snapshot State Reader.
		* @return a new Composite State Reader object.
		*/
  def composite(blockDiff: BlockDiff, inner: SnapshotStateReader): SnapshotStateReader = new CompositeStateReader(inner, blockDiff)

	/** Generates Snapshot State Reader given a Sequence of BlockDiff recursively.
		* @param blockDiff The Sequence of BlockDiff
		* @param inner The internal Snapshot State Reader.
		* @return A Snapshot State Reader.
		*/
	//TODO: Transform in tailrec.
  def composite(blockDiff: Seq[BlockDiff], inner: SnapshotStateReader): SnapshotStateReader = blockDiff match {
    case (x :: xs) => composite(x, composite(xs, inner))
    case _ => inner
  }

	/**  Generates Snapshot State Reader given a Non Empty List of BlockDiff recursively.
		* @param blockDiffs The NEL of BlockDiff
		* @param inner The internal Snapshot State Reader.
		* @return A Snapshot State Reader.
		*/
	//TODO: Transform in tailrec.
  def composite(blockDiffs: NEL[BlockDiff], inner: SnapshotStateReader): SnapshotStateReader = blockDiffs.tail match {
    case (x :: xs) => composite(blockDiffs.head, composite(NEL(x, xs), inner))
    case Nil => composite(blockDiffs.head, inner)
  }

	/** Generates Snapshot State Reader given a Coeval of BlockDiff.
		* @param blockDiff A Coeval of BlockDiff.
		* @param inner A Coeval of SnapshotStateReader.
		* @return A Coeval of SnapshotStateReader.
		*/
  // fresh head
  //TODO: Transform in tailrec.
  def composite(blockDiff: Coeval[BlockDiff], inner: Coeval[SnapshotStateReader]): Coeval[SnapshotStateReader] = for {
    i <- inner
    b <- blockDiff
  } yield composite(b, i)
}
