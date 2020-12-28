package io.lunes.transaction

import io.lunes.state2.ByteStr
import io.lunes.utils.HeightInfo
import monix.reactive.Observable
import scorex.block.Block.BlockId
import scorex.block.{Block, MicroBlock}
import io.lunes.transaction.History.BlockchainScore
import scorex.utils.Synchronized

/** Provides an interface for a Blockchain Updater. */
trait BlockchainUpdater extends Synchronized {
  /** Processes an input Block.
    * @param block The input Block.
    * @return Returns Either an Option for DiscardedTransactions (case Success) or a ValidationError (case Failure).
    */
  def processBlock(block: Block): Either[ValidationError, Option[DiscardedTransactions]]

  /** Processes an input MicroBlock.
    * @param microBlock The input MicroBlock.
    * @return Returns Either a ValidationError case it fails.
    */
  def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit]

  /** Remove blocks after a given Blcok ID.
    * @param blockId The ID for the block to remove after.
    * @return Returns Either Discarded Blocks (case Success) or a ValidationError (case Failure).
    */
  def removeAfter(blockId: ByteStr): Either[ValidationError, DiscardedBlocks]

  /** Gets the Last Block Information.
    * @return Returns an Observable for LastBlockInfo.
    */
  def lastBlockInfo: Observable[LastBlockInfo]

  /** Shuts Down the updater.
    */
  def shutdown(): Unit
}

/** Provides an interface for Blockchain Debug Information.  */
trait BlockchainDebugInfo {
  /** Returns the Height for a Lock Free State.
    * @return Returns a HeightInfo.
    */
  def lockfreeStateHeight: HeightInfo
}

/** Case Class for holding Last Block Information.
  * @constructor Creates a new data object for Last Block Information.
  * @param id The Block's ID.
  * @param height The Block's Height.
  * @param score The Score of the Block in the Blcokchain.
  * @param ready Block is ready.
  */
case class LastBlockInfo(id: BlockId, height: Int, score: BlockchainScore, ready: Boolean)
