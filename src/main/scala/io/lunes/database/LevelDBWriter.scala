package io.lunes.database

import io.lunes.settings.FunctionalitySettings
import io.lunes.state2._
import io.lunes.state2.reader.LeaseDetails
import org.iq80.leveldb.{DB, ReadOptions}
import scorex.account.{Address, Alias}
import scorex.block.{Block, BlockHeader}
import io.lunes.transaction.ValidationError.{AliasNotExists, AliasIsDisabled}
import io.lunes.transaction._
import io.lunes.transaction.lease.LeaseTransaction
import scorex.utils.ScorexLogging
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable

object LevelDBWriter {
  private def loadLeaseStatus(db: ReadOnlyDB, leaseId: ByteStr): Boolean =
    db.get(Keys.leaseStatusHistory(leaseId)).headOption.fold(false)(h => db.get(Keys.leaseStatus(leaseId)(h)))

  /** {{{
    * ([10, 7, 4], 5, 11) => [10, 7, 4]
    * ([10, 7], 5, 11) => [10, 7, 1]
    * }}}
    */
  private[database] def slice(v: Seq[Int], from: Int, to: Int): Seq[Int] = {
    val (c1, c2) = v.dropWhile(_ > to).partition(_ > from)
    c1 :+ c2.headOption.getOrElse(1)
  }

  /** {{{([15, 12, 3], [12, 5]) => [(15, 12), (12, 12), (3, 12), (3, 5)]}}}
    *
    * @param wbh WAVES balance history
    * @param lbh Lease balance history
    */
  private[database] def merge(wbh: Seq[Int], lbh: Seq[Int]): Seq[(Int, Int)] = {
    @tailrec
    def recMerge(wh: Int, wt: Seq[Int], lh: Int, lt: Seq[Int], buf: ArrayBuffer[(Int, Int)]): ArrayBuffer[(Int, Int)] = {
      buf += wh -> lh
      if (wt.isEmpty && lt.isEmpty) {
        buf
      } else if (wt.isEmpty) {
        recMerge(wh, wt, lt.head, lt.tail, buf)
      } else if (lt.isEmpty) {
        recMerge(wt.head, wt.tail, lh, lt, buf)
      } else {
        if (wh >= lh) {
          recMerge(wt.head, wt.tail, lh, lt, buf)
        } else {
          recMerge(wh, wt, lt.head, lt.tail, buf)
        }
      }
    }

    recMerge(wbh.head, wbh.tail, lbh.head, lbh.tail, ArrayBuffer.empty)
  }

  implicit class ReadOnlyDBExt(val db: ReadOnlyDB) extends AnyVal {
    def fromHistory[A](historyKey: Key[Seq[Int]], valueKey: Int => Key[A]): Option[A] =
      for {
        lastChange <- db.get(historyKey).headOption
      } yield db.get(valueKey(lastChange))

    def hasInHistory(historyKey: Key[Seq[Int]], v: Int => Key[_]): Boolean =
      db.get(historyKey)
        .headOption
        .exists(h => db.has(v(h)))
  }

  implicit class RWExt(val db: RW) extends AnyVal {
    def fromHistory[A](historyKey: Key[Seq[Int]], valueKey: Int => Key[A]): Option[A] =
      for {
        lastChange <- db.get(historyKey).headOption
      } yield db.get(valueKey(lastChange))
  }
}

class LevelDBWriter(writableDB: DB, fs: FunctionalitySettings, val maxCacheSize: Int = 100000) extends Caches with ScorexLogging {

  import LevelDBWriter._

  private def readOnly[A](f: ReadOnlyDB => A): A = {
    val s = writableDB.getSnapshot
    try f(new ReadOnlyDB(writableDB, new ReadOptions().snapshot(s)))
    finally s.close()
  }

  override protected def loadMaxAddressId(): BigInt = readOnly(db => db.get(Keys.lastAddressId).getOrElse(BigInt(0)))

  override protected def loadAddressId(address: Address): Option[BigInt] = readOnly(db => db.get(Keys.addressId(address)))

  override protected def loadHeight(): Int = readOnly(_.get(Keys.height))

  override protected def loadScore(): BigInt = readOnly(db => db.get(Keys.score(db.get(Keys.height))))

  override protected def loadLastBlock(): Option[Block] = readOnly(db => db.get(Keys.blockAt(db.get(Keys.height))))

  override def balance(address: Address, mayBeAssetId: Option[AssetId]): Long = readOnly { db =>
    addressId(address).fold(0L) { addressId =>
      mayBeAssetId match {
        case Some(assetId) => db.fromHistory(Keys.assetBalanceHistory(addressId, assetId), Keys.assetBalance(addressId, assetId)).getOrElse(0L)
        case None          => db.fromHistory(Keys.lunesBalanceHistory(addressId), Keys.lunesBalance(addressId)).getOrElse(0L)
      }
    }
  }

  override protected def loadApprovedFeatures(): Map[Short, Int] = readOnly(_.get(Keys.approvedFeatures))

  override protected def loadActivatedFeatures(): Map[Short, Int] = fs.preActivatedFeatures ++ readOnly(_.get(Keys.activatedFeatures))

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] = readOnly(db => db.get(Keys.transactionInfo(id)))

  override def transactionHeight(id: ByteStr): Option[Int] = readOnly(db => db.get(Keys.transactionHeight(id)))

  override def addressTransactions(address: Address, types: Set[Byte], count: Int, from: Int): Seq[(Int, Transaction)] = readOnly { db =>
    db.get(Keys.addressId(address)).fold(Seq.empty[(Int, Transaction)]) { addressId =>
      val txs = for {
        seqNr          <- (db.get(Keys.addressTransactionSeqNr(addressId)) to 1 by -1).view
        (txType, txId) <- db.get(Keys.addressTransactionIds(addressId, seqNr))
        if types.isEmpty || types.contains(txType.toByte)
        (h, tx) <- db.get(Keys.transactionInfo(txId))
      } yield (h, tx)

      txs.slice(from, count).force
    }
  }

  override def resolveAlias(alias: Alias): Either[ValidationError, Address] = readOnly { db =>
    if (db.get(Keys.aliasIsDisabled(alias))) Left(AliasIsDisabled(alias))
    else
      db.get(Keys.addressIdOfAlias(alias))
        .map(addressId => db.get(Keys.idToAddress(addressId)))
        .toRight(AliasNotExists(alias))
  }

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = readOnly { db =>
    db.get(Keys.transactionInfo(leaseId)) match {
      case Some((h, lt: LeaseTransaction)) =>
        Some(LeaseDetails(lt.sender, lt.recipient, h, lt.amount, loadLeaseStatus(db, leaseId)))
      case _ => None
    }
  }

  override def allActiveLeases: Set[LeaseTransaction] = readOnly { db =>
    val txs = for {
      h  <- 1 to db.get(Keys.height)
      id <- db.get(Keys.transactionIdsAtHeight(h))
      if loadLeaseStatus(db, id)
      (_, tx) <- db.get(Keys.transactionInfo(id))
    } yield tx

    txs.collect { case lt: LeaseTransaction => lt }.toSet
  }

  override def scoreOf(blockId: ByteStr): Option[BigInt] = readOnly(db => db.get(Keys.heightOf(blockId)).map(h => db.get(Keys.score(h))))

  override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = readOnly(_.get(Keys.blockHeader(height)))

  override def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)] =
    readOnly(db => db.get(Keys.heightOf(blockId)).flatMap(h => db.get(Keys.blockHeader(h))))

  override def blockBytes(height: Int): Option[Array[Byte]] = readOnly(_.get(Keys.blockBytes(height)))

  override def blockBytes(blockId: ByteStr): Option[Array[Byte]] =
    readOnly(db => db.get(Keys.heightOf(blockId)).flatMap(h => db.get(Keys.blockBytes(h))))

  override def heightOf(blockId: ByteStr): Option[Int] = readOnly(_.get(Keys.heightOf(blockId)))

  override def lastBlockIds(howMany: Int): immutable.IndexedSeq[ByteStr] = readOnly { db =>
    // since this is called from outside of the main blockchain updater thread, instead of using cached height,
    // explicitly read height from storage to make this operation atomic.
    val currentHeight = db.get(Keys.height)
    (currentHeight until (currentHeight - howMany).max(0) by -1)
      .map(h => db.get(Keys.blockHeader(h)).get._1.signerData.signature)
  }

  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = readOnly { db =>
    db.get(Keys.heightOf(parentSignature)).map { parentHeight =>
      (parentHeight until (parentHeight + howMany))
        .flatMap { h =>
          db.get(Keys.blockHeader(h))
        }
        .map { b =>
          b._1.signerData.signature
        }
    }
  }

  override def parent(block: Block, back: Int): Option[Block] = readOnly { db =>
    db.get(Keys.heightOf(block.reference)).flatMap(h => db.get(Keys.blockAt(h - back + 1)))
  }

  override def assetDistribution(assetId: ByteStr): Map[Address, Long] = readOnly { db =>
    (for {
      seqNr     <- (1 to db.get(Keys.addressesForAssetSeqNr(assetId))).par
      addressId <- db.get(Keys.addressesForAsset(assetId, seqNr)).par
      balance   <- db.fromHistory(Keys.assetBalanceHistory(addressId, assetId), Keys.assetBalance(addressId, assetId))
      if balance > 0
    } yield db.get(Keys.idToAddress(addressId)) -> balance).toMap.seq
  }

  override def lunesDistribution(height: Int): Map[Address, Long] = readOnly { db =>
    (for {
      seqNr     <- (1 to db.get(Keys.addressesForLunesSeqNr)).par
      addressId <- db.get(Keys.addressesForLunes(seqNr)).par
      history = db.get(Keys.lunesBalanceHistory(addressId))
      actualHeight <- history.partition(_ > height)._2.headOption
      balance = db.get(Keys.lunesBalance(addressId)(actualHeight))
      if balance > 0
    } yield db.get(Keys.idToAddress(addressId)) -> balance).toMap.seq
  }
}