package io.lunes.state2

import io.lunes.utx.UtxPool
import io.lunes.features.FeatureProvider
import io.lunes.mining._
import io.lunes.network._
import io.lunes.settings.{FunctionalitySettings, LunesSettings}
import io.lunes.state2.reader.SnapshotStateReader
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.eval.Task
import scorex.block.Block
import scorex.consensus.TransactionsOrdering
import io.lunes.transaction.PoSCalc._
import io.lunes.transaction.ValidationError.{BlockFromFuture, GenericError}
import io.lunes.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.util.{Left, Right}

/**  Package object */
package object appender extends ScorexLogging {

  private val MaxTimeDrift: Long = 100 // millis

  private val correctBlockId1 = ByteStr.decodeBase58("2GNCYVy7k3kEPXzz12saMtRDeXFKr8cymVsG8Yxx3sZZ75eHj9csfXnGHuuJe7XawbcwjKdifUrV1uMq4ZNCWPf1").get
  private val correctBlockId2 = ByteStr.decodeBase58("5uZoDnRKeWZV9Thu2nvJVZ5dBvPB7k2gvpzFD618FMXCbBVBMN2rRyvKBZBhAGnGdgeh2LXEeSr9bJqruJxngsE7").get
  private val height1 = 812608
  private val height2 = 813207

  /** Package restricted method for Process and add to a Blacklist on Failure.
    *
    * @param ch Inputs a Netty Channel.
    * @param peerDatabase The PeerDatabase.
    * @param miner The Miner object.
    * @param allChannels The Netty Channel Group.
    * @param start A function into String for the Starting Process.
    * @param success A function into String for the Success case.
    * @param errorPrefix The error Prefix.
    * @param f A function into Task of Either Option of BigInt (case success) or type Parameter B (case Failure).
    * @tparam A The Parametrized type A.
    * @tparam B The Parametrized type B.
    * @return Returns a Task for Either an Option of BigInt (case Success) or B (case Failure).
    */
  private[appender] def processAndBlacklistOnFailure[A, B](ch: Channel, peerDatabase: PeerDatabase, miner: Miner, allChannels: ChannelGroup,start: => String, success: => String, errorPrefix: String)(f: => Task[Either[B, Option[BigInt]]]): Task[Either[B, Option[BigInt]]] = {

    log.debug(start)
    f map {
      case Right(maybeNewScore) =>
        log.debug(success)
        maybeNewScore.foreach(_ => miner.scheduleMining())
        Right(maybeNewScore)
      case Left(ve) =>
        log.warn(s"$errorPrefix: $ve")
        peerDatabase.blacklistAndClose(ch, s"$errorPrefix: $ve")
        Left(ve)
    }
  }

  /** Validate Effective Balance
    * @param fp Inputs a Feture Provider.
    * @param fs Inputs a Functionality Settings.
    * @param block Inputs a Block.
    * @param baseHeight Inputs a base height.
    * @param effectiveBalance Inputs a Effective Balance to validate.
    * @return Returns Either a Long (case Success) or String (case Failure).
    */
  private def validateEffectiveBalance(fp: FeatureProvider, fs: FunctionalitySettings, block: Block, baseHeight: Int)(effectiveBalance: Long): Either[String, Long] =
    Either.cond(block.timestamp < fs.minimalGeneratingBalanceAfter ||
      (block.timestamp >= fs.minimalGeneratingBalanceAfter && effectiveBalance >= MinimalEffectiveBalanceForGenerator), 
      effectiveBalance, s"generator's effective balance $effectiveBalance is less that required for generation")

	/** Package Restricted method for append Block.
		* @param checkpoint Inputs a Checkpoint Service.
		* @param history Inputs a History.
		* @param blockchainUpdater Inputs a Blockchain Updater.
		* @param stateReader Inputs a Snapshot State Reader.
		* @param utxStorage Inputs a Transaction Pool.
		* @param time Sets a Time.
		* @param settings Inputs a Lunes Settings.
		* @param featureProvider Inputs a Feature Provider.
		* @param block The input Block.
		* @return Returns Either a Option for Int (case Success) or a ValidationError (case Failure).
		*/
  private[appender] def appendBlock(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater,
                                    stateReader: SnapshotStateReader, utxStorage: UtxPool, time: Time, settings: LunesSettings,
                                    featureProvider: FeatureProvider)(block: Block): Either[ValidationError, Option[Int]] = for {
    _ <- Either.cond(checkpoint.isBlockValid(block.signerData.signature, history.height() + 1), (),
      GenericError(s"Block $block at height ${history.height() + 1} is not valid w.r.t. checkpoint"))
    _ <- blockConsensusValidation(history, featureProvider, settings, time.correctedTime(), block) { height =>
      PoSCalc.generatingBalance(stateReader, settings.blockchainSettings.functionalitySettings, block.signerData.generator, height).toEither.left.map(_.toString)
        .flatMap(validateEffectiveBalance(featureProvider, settings.blockchainSettings.functionalitySettings, block, height))
    }
    baseHeight = history.height()
    maybeDiscardedTxs <- blockchainUpdater.processBlock(block)
  } yield {
    utxStorage.removeAll(block.transactionData)
    maybeDiscardedTxs.toSeq.flatten.foreach(utxStorage.putIfNew)
    maybeDiscardedTxs.map(_ => baseHeight)
  }

	/** Validate a Block Consensus.
		* @param history Inputs a History.
		* @param fp Inputs a Feature Provider.
		* @param settings Inputs a Lunes Settings.
		* @param currentTs Inputs the current Timestamp.
		* @param block The input Block.
		* @param genBalance The generated Balance. It is a Function from Int to Either a Long (case Success) or a String (case Failure).
		* @return Returns Either a Unit (case Success) or a ValidationError (case Failure).
		*/
  private def blockConsensusValidation(history: History, fp: FeatureProvider, settings: LunesSettings, currentTs: Long, block: Block)
                                      (genBalance: Int => Either[String, Long]): Either[ValidationError, Unit] = history.read { _ =>

    val bcs = settings.blockchainSettings
    val fs = bcs.functionalitySettings
    val blockTime = block.timestamp
    val generator = block.signerData.generator

    val r: Either[ValidationError, Unit] = for {
      height <- history.heightOf(block.reference).toRight(GenericError(s"history does not contain parent ${block.reference}"))
      _ <- Either.cond(height > fs.blockVersion3AfterHeight
        || block.version == Block.GenesisBlockVersion
        || block.version == Block.PlainBlockVersion,
        (), GenericError(s"Block Version 3 can only appear at height greater than ${fs.blockVersion3AfterHeight}"))
      _ <- Either.cond(blockTime - currentTs < MaxTimeDrift, (), BlockFromFuture(blockTime))
      _ <- {
        val constraints = MiningEstimators(settings.minerSettings, fp, height)
        Either.cond(!OneDimensionalMiningConstraint.full(constraints.total).put(block).isOverfilled, (), GenericError("Block is full"))
      }
      _ <- Either.cond(blockTime < fs.requireSortedTransactionsAfter
        || height > fs.dontRequireSortedTransactionsAfter
        || block.transactionData.sorted(TransactionsOrdering.InBlock) == block.transactionData,
        (), GenericError("transactions are not sorted"))
      parent <- history.parent(block).toRight(GenericError(s"history does not contain parent ${block.reference}"))
      prevBlockData = parent.consensusData
      blockData = block.consensusData
      cbt = calcBaseTarget(bcs.genesisSettings.averageBlockDelay, height, parent.consensusData.baseTarget, parent.timestamp, history.parent(parent, 2).map(_.timestamp), blockTime)
      bbt = blockData.baseTarget
      _ <- Either.cond(cbt == bbt, (), GenericError(s"declared baseTarget $bbt does not match calculated baseTarget $cbt"))
      calcGs = calcGeneratorSignature(prevBlockData, generator)
      blockGs = blockData.generationSignature.arr
      _ <- Either.cond(calcGs.sameElements(blockGs), (), GenericError(s"declared generation signature ${blockGs.mkString} does not match calculated generation signature ${calcGs.mkString}"))
      effectiveBalance <- genBalance(height).left.map(GenericError(_))
      hit = calcHit(prevBlockData, generator)
      target = calcTarget(parent.timestamp, parent.consensusData.baseTarget, blockTime, effectiveBalance)
      _ <- Either.cond(hit < target || (height == height1 && block.uniqueId == correctBlockId1) || (height == height2 && block.uniqueId == correctBlockId2),
        (), GenericError(s"calculated hit $hit >= calculated target $target"))
    } yield ()

    r.left.map {
      case GenericError(x) => GenericError(s"Block $block is invalid: $x")
      case x => x
    }
  }

}
