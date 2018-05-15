package io.lunes.state2.reader

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats._
import cats.implicits._
import io.lunes.state2._
import scorex.account.{Address, Alias}
import io.lunes.transaction.lease.LeaseTransaction
import io.lunes.transaction.{Transaction, TransactionParser}

/** Implements a Snapshot State Reader.
  * @param p The State Storage database.
  * @param synchronizationToken The Token for Synchronization.
  */
class StateReaderImpl(p: StateStorage, val synchronizationToken: ReentrantReadWriteLock) extends SnapshotStateReader {

  val sp = Synchronized(p)

  /**
    * @param id Inputs the Transaction ID.
    * @return Returns an Option for a Tuple (Int, Option[Transaction]).
    */
  override def transactionInfo(id: ByteStr): Option[(Int, Option[Transaction])] = read { implicit l =>
    sp().getTransaction(id).map {
      case (h, bytes) => (h, if (bytes.length == 0) None else TransactionParser.parseBytes(bytes).toOption)
    }
  }

  /**
    * @param a Inputs an [[scorex.account.Address]].
    * @return Returns the Portfolio object.
    */
  override def accountPortfolio(a: Address): Portfolio = read { implicit l =>
    val lunes = sp().getLunesBalance(a).map { case (b, li, lo) => Portfolio(b, LeaseInfo(li, lo), Map.empty) }.orEmpty
    val assets = sp().getAssetBalanceMap(a).map { f => Portfolio(0, LeaseInfo.empty, f) }.orEmpty

    Monoid.combine(lunes, assets)
  }

  /**
    * @param id Inputs Asset ID.
    * @return Returns an Option for [[io.lunes.state2.AssetInfo]].
    */
  override def assetInfo(id: ByteStr): Option[AssetInfo] = read { implicit l =>
    sp().getAssetInfo(id)
  }

  /**
    * @return Returns the height.
    */
  override def height: Int = read { implicit l => sp().getHeight }

  override def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr] = read { implicit l =>
    val totalRecords = sp().getAccountTransactionsLengths(a).getOrElse(0)
    Range(Math.max(0, totalRecords - limit), totalRecords)
      .map(n => sp().getAccountTransactionIds(a, n).get)
      .reverse
  }

  /**
    * @param address The input [[Address]].
    * @return Returns a Sequence of [[scorex.account.Alias]].
    */
  override def aliasesOfAddress(address: Address): Seq[Alias] = read { implicit l =>
    sp().getAddressAliases(address).getOrElse(Seq.empty)
  }

  /**
    * @param alias The [[Alias]] to resolve.
    * @return Returns an Option for [[scorex.account.Address]].
    */
  override def resolveAlias(alias: Alias): Option[Address] = read { implicit l =>
    sp().getAddressOfAlias(alias)
  }

  /**
    * @return Returns a Map for [[scorex.account.Address]] into [[io.lunes.state2.Portfolio]].
    */
  override def accountPortfolios: Map[Address, Portfolio] = read { implicit l =>
    sp().allPortfolios
  }

  /**
    * @param leaseTx Inpust a [[io.lunes.transaction.lease.LeaseTransaction]].
    * @return Returns true if the Lease is active.
    */
  override def isLeaseActive(leaseTx: LeaseTransaction): Boolean = read { implicit l =>
    sp().getLeaseState(leaseTx.id()).getOrElse(false)
  }

  /**
    * @return Returns a Sequence of IDs for Active Leases.
    */
  override def activeLeases(): Seq[ByteStr] = read { implicit l =>
    sp().getActiveLeases.getOrElse(Seq.empty[ByteStr])
  }

  /**
    * @param acc Inputs an [[scorex.account.Address]].
    * @return Returns an Option for the Height.
    */
  override def lastUpdateHeight(acc: Address): Option[Int] = read { implicit l =>
    sp().getLastBalanceSnapshotHeight(acc)
  }

  /**
    * @param acc Inputs the [[scorex.account.Address]].
    * @param h Inputs the Height.
    * @return Returns an Option for [[io.lunes.state2.Snapshot]].
    */
  override def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot] = read { implicit l =>
    sp().getBalanceSnapshots(acc, h).map { case (ph, b, eb) => Snapshot(ph, b, eb) }
  }

  /**
    * @param id Inputs the Transaction ID.
    * @return Return true if exists.
    */
  override def containsTransaction(id: ByteStr): Boolean = read { implicit l =>
    sp().getTransaction(id).isDefined
  }

  /**
    * @param orderId Inpúts the order ID.
    * @return Return an [[io.lunes.state2.OrderFillInfo]].
    */
  override def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo = read { implicit l =>
    sp().getOrderFills(orderId).getOrElse(Monoid[OrderFillInfo].empty)
  }

  /**
    * @param a Inputs an [[scorex.account.Address]].
    * @return Returns a Tuple (Long, LeaseInfo).
    */
  override def lunesBalance(a: Address): (Long, LeaseInfo) = read { implicit l =>
    sp().getLunesBalance(a).map { case (v1, v2, v3) => (v1, LeaseInfo(v2, v3)) }.getOrElse((0L, LeaseInfo(0L, 0L)))
  }

  /**
    * @param a Inputs an [[scorex.account.Address]]
    * @param asset Inputs Asset ID.
    * @return Returns the Asset Balance.
    */
  override def assetBalance(a: Address, asset: ByteStr): Long = read { implicit l =>
    sp().getAssetBalance(a, asset).getOrElse(0L)
  }
}
