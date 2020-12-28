package io.lunes.state2.appender

import io.lunes.utx.UtxPool
import io.lunes.features.FeatureProvider
import io.lunes.metrics.{BlockStats, Instrumented, Metrics}
import io.lunes.mining.Miner
import io.lunes.network.{InvalidBlockStorage, PeerDatabase, formatBlocks, id}
import io.lunes.settings.LunesSettings
import io.lunes.state2._
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import org.influxdb.dto.Point
import scorex.block.Block
import io.lunes.transaction.ValidationError.GenericError
import io.lunes.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.util.{Left, Right}

/** Extension Appender object. */
object ExtensionAppender extends ScorexLogging with Instrumented {
  /** Factory method for a Task for Either an Option for a BigInt (case Success) or a ValidationError (case Failure).
    * @param checkpoint The Checkpoint Object.
    * @param history The input History object.
    * @param blockchainUpdater The Blockchain Updater.
    * @param stateReader The State Reader.
    * @param utxStorage The Transaction Pool.
    * @param time The Time for the Block.
    * @param settings The Lunes Settings object.
    * @param featureProvider The Feature Provider.
    * @param invalidBlocks The Invalid Blocks Storage.
    * @param peerDatabase The PeerDatabase.
    * @param miner The input Miner.
    * @param allChannels The Channel Group.
    * @param scheduler The Scheduler.
    * @param ch The Netty Channel.
    * @param extensionBlocks The Sequence of Extensions Blocks.
    * @return Returns a Task for Either an Option of BigInt (case Success) or a ValidationError (case Failure).
    */
  def apply(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater,
            stateReader: StateReader, utxStorage: UtxPool, time: Time, settings: LunesSettings,
            featureProvider: FeatureProvider, invalidBlocks: InvalidBlockStorage,
            peerDatabase: PeerDatabase, miner: Miner, allChannels: ChannelGroup, scheduler: Scheduler
           )(ch: Channel, extensionBlocks: Seq[Block]): Task[Either[ValidationError, Option[BigInt]]] = {
    def p(blocks: Seq[Block]): Task[Either[ValidationError, Option[BigInt]]] = Task(Signed.validateOrdered(blocks).flatMap { newBlocks =>
      history.write("apply") { implicit l =>
        val extension = newBlocks.dropWhile(history.contains)

        extension.headOption.map(_.reference) match {
          case Some(lastCommonBlockId) =>
            def isForkValidWithCheckpoint(lastCommonHeight: Int): Boolean =
              extension.zipWithIndex.forall(p => checkpoint.isBlockValid(p._1.signerData.signature, lastCommonHeight + 1 + p._2))

            val forkApplicationResultEi = Coeval {
              extension.view
                .map { b =>
                  b -> appendBlock(checkpoint, history, blockchainUpdater, stateReader(), utxStorage, time, settings, featureProvider)(b).right.map {
                    _.foreach(bh => BlockStats.applied(b, BlockStats.Source.Ext, bh))
                  }
                }
                .zipWithIndex
                .collectFirst { case ((b, Left(e)), i) => (i, b, e) }
                .fold[Either[ValidationError, Unit]](Right(())) {
                case (i, declinedBlock, e) =>
                  e match {
                    case _: ValidationError.BlockFromFuture =>
                    case _ => invalidBlocks.add(declinedBlock.uniqueId, e)
                  }

                  extension.view
                    .dropWhile(_ != declinedBlock)
                    .foreach(BlockStats.declined(_, BlockStats.Source.Ext))

                  if (i == 0) log.warn(s"Can't process fork starting with $lastCommonBlockId, error appending block $declinedBlock: $e")
                  else log.warn(s"Processed only ${i + 1} of ${newBlocks.size} blocks from extension, error appending next block $declinedBlock: $e")

                  Left(e)
              }
            }

            val initalHeight = history.height()

            val droppedBlocksEi = for {
              commonBlockHeight <- history.heightOf(lastCommonBlockId).toRight(GenericError("Fork contains no common parent"))
              _ <- Either.cond(isForkValidWithCheckpoint(commonBlockHeight), (), GenericError("Fork contains block that doesn't match checkpoint, declining fork"))
              droppedBlocks <- blockchainUpdater.removeAfter(lastCommonBlockId)
            } yield (commonBlockHeight, droppedBlocks)

            droppedBlocksEi.flatMap {
              case (commonBlockHeight, droppedBlocks) =>
                forkApplicationResultEi() match {
                  case Left(e) =>
                    blockchainUpdater.removeAfter(lastCommonBlockId).explicitGet()
                    droppedBlocks.foreach(blockchainUpdater.processBlock(_).explicitGet())
                    Left(e)

                  case Right(_) =>
                    val depth = initalHeight - commonBlockHeight
                    if (depth > 0) {
                      Metrics.write(
                        Point
                          .measurement("rollback")
                          .addField("depth", initalHeight - commonBlockHeight)
                          .addField("txs", droppedBlocks.size)
                      )
                    }
                    droppedBlocks.flatMap(_.transactionData).foreach(utxStorage.putIfNew)
                    Right(Some(history.score()))
                }
            }

          case None =>
            log.debug("No new blocks found in extension")
            Right(None)
        }
      }
    }).executeOn(scheduler)

    extensionBlocks.foreach(BlockStats.received(_, BlockStats.Source.Ext, ch))
    processAndBlacklistOnFailure(ch, peerDatabase, miner, allChannels,
      s"${id(ch)} Attempting to append extension ${formatBlocks(extensionBlocks)}",
      s"${id(ch)} Successfully appended extension ${formatBlocks(extensionBlocks)}",
      s"${id(ch)} Error appending extension ${formatBlocks(extensionBlocks)}"
    )(p(extensionBlocks))
  }
}
