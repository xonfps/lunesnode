package io.lunes.state2

import io.lunes.state2.reader.LeaseDetails
import scorex.account.{Address, Alias}
import scorex.block.{Block, BlockHeader}
import io.lunes.transaction.lease.LeaseTransaction
import io.lunes.transaction.{AssetId, Transaction, ValidationError}

trait Blockchain {
  def height: Int
  def score: BigInt
  def scoreOf(blockId: ByteStr): Option[BigInt]

  def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)]
  def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)]

  def lastBlock: Option[Block]
  def blockBytes(height: Int): Option[Array[Byte]]
  def blockBytes(blockId: ByteStr): Option[Array[Byte]]

  def heightOf(blockId: ByteStr): Option[Int]

  /** Returns the most recent block IDs, starting from the most recent  one */
  def lastBlockIds(howMany: Int): Seq[ByteStr]

  /** Returns a chain of blocks starting with the block with the given ID (from oldest to newest) */
  def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]]

  def parent(block: Block, back: Int = 1): Option[Block]

  /** Features related */
  def approvedFeatures: Map[Short, Int]
  def activatedFeatures: Map[Short, Int]

  def transactionInfo(id: ByteStr): Option[(Int, Transaction)]
  def transactionHeight(id: ByteStr): Option[Int]

  def addressTransactions(address: Address, types: Set[Byte], count: Int, from: Int): Seq[(Int, Transaction)]

  def containsTransaction(id: ByteStr): Boolean
  def forgetTransactions(pred: (ByteStr, Long) => Boolean): Map[ByteStr, Long]
  def learnTransactions(values: Map[ByteStr, Long]): Unit

  def resolveAlias(a: Alias): Either[ValidationError, Address]

  def leaseDetails(leaseId: ByteStr): Option[LeaseDetails]

  def balance(address: Address, mayBeAssetId: Option[AssetId]): Long

  def assetDistribution(assetId: ByteStr): Map[Address, Long]
  def lunesDistribution(height: Int): Map[Address, Long]

  // the following methods are used exclusively by patches
  def allActiveLeases: Set[LeaseTransaction]
}