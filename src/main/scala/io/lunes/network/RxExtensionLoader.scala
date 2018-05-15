package io.lunes.network

import io.lunes.network.RxExtensionLoader.ApplierState.Buffer
import io.lunes.network.RxExtensionLoader.LoaderState.WithPeer
import io.lunes.network.RxScoreObserver.{ChannelClosedAndSyncWith, SyncWith}
import io.netty.channel._
import monix.eval.{Coeval, Task}
import monix.execution.CancelableFuture
import monix.execution.schedulers.SchedulerService
import monix.reactive.subjects.{ConcurrentSubject, Subject}
import monix.reactive.{Observable, Observer}
import scorex.block.Block
import scorex.block.Block.BlockId
import io.lunes.transaction.History.BlockchainScore
import io.lunes.transaction.ValidationError.GenericError
import io.lunes.transaction.{NgHistory, ValidationError}
import scorex.utils.ScorexLogging

import scala.concurrent.duration._

/** The RxExtension Loader object. */
object RxExtensionLoader extends ScorexLogging {

  /** Asserts a type for Apply */
  type ApplyExtensionResult = Either[ValidationError, Option[BlockchainScore]]

  /** Factory for a Tuple of Observable[(Channel, Block)], Coeval[State], RxExtensionLoaderShutdownHook.
    * @param maxRollback Maximum Rollback.
    * @param syncTimeOut Synchronization Timeout
    * @param history The NgHistory object.
    * @param peerDatabase PeerDatabase input.
    * @param invalidBlocks The Ivalid Block Storage.
    * @param blocks Inputs an Observable Tuple(Channel, Block).
    * @param signatures Inputs an Observable Tuple(Channel, Block).
    * @param syncWithChannelClosed Inputs an Observable of ChannelCloseAndSyncWith.
    * @param scheduler The Scheduler Service object.
    * @param timeoutSubject Inputs and Subject for Channel and Channel.
    * @param extensionApplier Inputs a Map for Tuple(Channel, ExtensionBlock) into a Task of ApplyExtensionResult.
    * @return Returns a Tuple(Observable[(Channel, Block)], Coeval[State], RxExtensionLoaderShutdownHook)
    */
  def apply(maxRollback: Int, syncTimeOut: FiniteDuration,
            history: NgHistory,
            peerDatabase: PeerDatabase,
            invalidBlocks: InvalidBlockStorage,
            blocks: Observable[(Channel, Block)],
            signatures: Observable[(Channel, Signatures)],
            syncWithChannelClosed: Observable[ChannelClosedAndSyncWith],
            scheduler: SchedulerService,
            timeoutSubject: Subject[Channel, Channel])
           (extensionApplier: (Channel, ExtensionBlocks) => Task[ApplyExtensionResult]): (Observable[(Channel, Block)], Coeval[State], RxExtensionLoaderShutdownHook) = {

    implicit val schdlr = scheduler

    val extensions: ConcurrentSubject[(Channel, ExtensionBlocks), (Channel, ExtensionBlocks)] = ConcurrentSubject.publish[(Channel, ExtensionBlocks)]
    val simpleBlocks: ConcurrentSubject[(Channel, Block), (Channel, Block)] = ConcurrentSubject.publish[(Channel, Block)]
    @volatile var s: State = State(LoaderState.Idle, ApplierState.Idle)
    val lastSyncWith: Coeval[Option[SyncWith]] = lastObserved(syncWithChannelClosed.map(_.syncWith))

    /**
      *
      * @param ch
      * @param reason
      * @return
      */
    def scheduleBlacklist(ch: Channel, reason: String): Task[Unit] = Task {
      timeoutSubject.onNext(ch)
      peerDatabase.blacklistAndClose(ch, reason)
    }.delayExecution(syncTimeOut)

    /**
      * @param state
      * @param syncWith
      * @return
      */
    def syncNext(state: State, syncWith: SyncWith = lastSyncWith().flatten): State =
      syncWith match {
        case None =>
          log.trace("Last bestChannel is None, state is up to date")
          state.withIdleLoader
        case Some(best) =>
          state.loaderState match {
            case wp: WithPeer =>
              log.trace(s"${id(wp.channel)} Already syncing, no need to sync next, $state")
              state
            case LoaderState.Idle =>
              val maybeKnownSigs = state.applierState match {
                case ApplierState.Idle => Some((history.lastBlockIds(maxRollback), false))
                case ApplierState.Applying(None, ext) => Some((ext.blocks.map(_.uniqueId), true))
                case _ => None
              }
              maybeKnownSigs match {
                case Some((knownSigs, optimistic)) =>
                  val ch = best.channel
                  log.debug(s"${id(ch)} Requesting extension signatures ${if (optimistic) "optimistically" else ""}, last ${knownSigs.length} are ${formatSignatures(knownSigs)}")
                  val blacklisting = scheduleBlacklist(ch, s"Timeout loading extension").runAsync
                  ch.writeAndFlush(GetSignatures(knownSigs))
                  state.withLoaderState(LoaderState.ExpectingSignatures(ch, knownSigs, blacklisting))
                case None =>
                  log.trace(s"Holding on requesting next sigs, $state")
                  state
              }
          }
      }

    /**
      *
      * @param state
      * @param cc
      * @return
      */
    def onNewSyncWithChannelClosed(state: State, cc: ChannelClosedAndSyncWith): State = {
      cc match {
        case ChannelClosedAndSyncWith(_, None) =>
          state.loaderState match {
            case _: LoaderState.WithPeer => state.withIdleLoader
            case _ => state
          }
        case ChannelClosedAndSyncWith(None, Some(bestChannel)) =>
          log.trace(s"New SyncWith: $bestChannel, currentState = $state")
          syncNext(state, Some(bestChannel))
        case ChannelClosedAndSyncWith(Some(closedChannel), Some(bestChannel)) =>
          state.loaderState match {
            case wp: LoaderState.WithPeer if closedChannel != wp.channel => state
            case _ => syncNext(state.withIdleLoader, Some(bestChannel))
          }
      }
    }

    /**
      *
      * @param state
      * @param ch
      * @param sigs
      * @return
      */
    def onNewSignatures(state: State, ch: Channel, sigs: Signatures): State = {
      state.loaderState match {
        case LoaderState.ExpectingSignatures(c, known, _) if c == ch =>
          val (_, unknown) = sigs.signatures.span(id => known.contains(id))

          val firstInvalid = sigs.signatures
            .view
            .flatMap { sig => invalidBlocks.find(sig).map(sig -> _) }
            .headOption

          firstInvalid match {
            case Some((invalidBlock, reason)) =>
              peerDatabase.blacklistAndClose(ch, s"Signatures contain invalid block(s): $invalidBlock, $reason")
              syncNext(state.withIdleLoader)
            case None =>
              if (unknown.isEmpty) {
                log.trace(s"${id(ch)} Received empty extension signatures list, sync with node complete")
                state.withIdleLoader
              } else {
                log.trace(s"${id(ch)} Requesting ${unknown.size} blocks")
                val blacklistingAsync = scheduleBlacklist(ch, "Timeout loading first requested block").runAsync
                unknown.foreach(s => ch.write(GetBlock(s)))
                ch.flush()
                state.withLoaderState(LoaderState.ExpectingBlocks(ch, unknown, unknown.toSet, Set.empty, blacklistingAsync))
              }
          }
        case _ =>
          log.trace(s"${id(ch)} Received unexpected signatures ${formatSignatures(sigs.signatures)}, ignoring at $state")
          state
      }
    }

    /**
      *
      * @param state
      * @param ch
      * @param block
      * @return
      */
    def onBlock(state: State, ch: Channel, block: Block): State = {
      state.loaderState match {
        case LoaderState.ExpectingBlocks(c, requested, expected, recieved, _) if c == ch && expected.contains(block.uniqueId) =>
          if (expected == Set(block.uniqueId)) {
            val blockById = (recieved + block).map(b => b.uniqueId -> b).toMap
            val ext = ExtensionBlocks(requested.map(blockById))
            log.debug(s"${id(ch)} $ext successfully received")
            extensionLoadingFinished(state.withIdleLoader, ext, ch)
          } else {
            val blacklistAsync = scheduleBlacklist(ch, s"Timeout loading one of requested blocks, non-received: ${
              val totalleft = expected.size - 1
              if (totalleft == 1) "one=" + requested.last.trim
              else "total=" + totalleft.toString
            }").runAsync
            state.withLoaderState(LoaderState.ExpectingBlocks(c, requested, expected - block.uniqueId, recieved + block, blacklistAsync))
          }
        case _ =>
          simpleBlocks.onNext((ch, block))
          state

      }
    }

    /**
      *
      * @param state
      * @param extension
      * @param ch
      * @return
      */
    def extensionLoadingFinished(state: State, extension: ExtensionBlocks, ch: Channel): State = {
      state.applierState match {
        case ApplierState.Idle =>
          extensions.onNext(ch -> extension)
          syncNext(state.copy(applierState = ApplierState.Applying(None, extension)))
        case s@ApplierState.Applying(None, applying) =>
          log.trace(s"An optimistic extension was received: $extension, but applying $applying now")
          state.copy(applierState = s.copy(buf = Some(Buffer(ch, extension))))
        case _ =>
          log.warn(s"Overflow, discarding $extension")
          state
      }
    }

    /**
      *
      * @param state
      * @param extension
      * @param ch
      * @param applicationResult
      * @return
      */
    def onExtensionApplied(state: State, extension: ExtensionBlocks, ch: Channel, applicationResult: ApplyExtensionResult): State = {
      log.trace(s"Applying $extension finished with $applicationResult")
      state.applierState match {
        case ApplierState.Idle =>
          log.warn(s"Applied $extension but ApplierState is Idle")
          state
        case ApplierState.Applying(maybeBuffer, applying) =>
          if (applying != extension) log.warn(s"Applied $extension doesn't match expected $applying")
          maybeBuffer match {
            case None => state.copy(applierState = ApplierState.Idle)
            case Some(Buffer(nextChannel, nextExtension)) => applicationResult match {
              case Left(_) =>
                log.debug(s"Failed to apply $extension, discarding cached as well")
                syncNext(state.copy(applierState = ApplierState.Idle))
              case Right(_) =>
                log.trace(s"Successfully applied $extension, starting to apply an optimistically loaded one: $nextExtension")
                extensions.onNext(nextChannel -> nextExtension)
                syncNext(state.copy(applierState = ApplierState.Applying(None, nextExtension)))
            }
          }
      }
    }

    /**
      *
      * @return
      */
    def appliedExtensions: Observable[(Channel, ExtensionBlocks, ApplyExtensionResult)] = {
      def apply(x: (Channel, ExtensionBlocks)): Task[ApplyExtensionResult] = Function.tupled(extensionApplier)(x)

      /**
        *
        */
      extensions.mapTask { x =>
        apply(x)
          .asyncBoundary(scheduler)
          .onErrorHandle { err =>
            log.error("Error during extension applying", err)
            Left(GenericError(err))
          }
          .map((x._1, x._2, _))
      }
    }

    Observable
      .merge(
        signatures.observeOn(scheduler).map { case ((ch, sigs)) => s = onNewSignatures(s, ch, sigs) },
        blocks.observeOn(scheduler).map { case ((ch, block)) => s = onBlock(s, ch, block) },
        syncWithChannelClosed.observeOn(scheduler).map { ch => s = onNewSyncWithChannelClosed(s, ch) },
        appliedExtensions.map { case ((ch, extensionBlocks, ar)) => s = onExtensionApplied(s, extensionBlocks, ch, ar) }
      )
      .map { _ => log.trace(s"Current state: $s") }
      .logErr
      .subscribe()

    (simpleBlocks, Coeval.eval(s), RxExtensionLoaderShutdownHook(extensions, simpleBlocks))
  }

  /** Defines a LoaderState interface. */
  sealed trait LoaderState

  /**  The LoaderState */
  object LoaderState {

    /** Defines a derived interface for LoaderState*/
    sealed trait WithPeer extends LoaderState {
      /** Defines an interface to retrieve a Netty Channel
        * @return Returns the Channel.
        */
      def channel: Channel

      /** Defines an interface for a Timeout Unit.
        * @return Returns a Cancelable Future for Unit.
        */
      def timeout: CancelableFuture[Unit]
    }

    /** Defines a case object derived from LoadState */
    case object Idle extends LoaderState

    /** Case Class to holds data for Expected Signatures.
      * @param channel Inputs the Netty Channel.
      * @param known Inputs a Sequence of Block IDs.
      * @param timeout Cancelable Future Unit.
      */
    case class ExpectingSignatures(channel: Channel, known: Seq[BlockId], timeout: CancelableFuture[Unit]) extends WithPeer {
      override def toString: String = s"ExpectingSignatures(channel=${id(channel)})"
    }

    /** Case Class for holding data for Expected Blocks.
      * @param channel Inputs the Netty Channel.
      * @param allBlocks Inputs a Sequence of all Block IDs.
      * @param expected Inputs a Set of the expected Block IDs.
      * @param received Inputs a Set of the received Block IDs.
      * @param timeout Cancelable Future Unit.
      */
    case class ExpectingBlocks(channel: Channel, allBlocks: Seq[BlockId],
                               expected: Set[BlockId],
                               received: Set[Block],
                               timeout: CancelableFuture[Unit]) extends WithPeer {
      override def toString: String = s"ExpectingBlocks(channel=${id(channel)}, totalBlocks=${allBlocks.size}, " +
        s"received=${received.size}, expected=${if (expected.size == 1) expected.head.trim else expected.size})"
    }

  }

  /** Reactive Extension Loader Shutdown Hook case class.
    * @constructor Creates a new Reactive Extension Shutdown Hook.
    * @param extensionChannel Inputs a Observer Tuple(Channel, ExtensionBlocks).
    * @param simpleBlocksChannel Inpts a Observer Tuple for (Channel, Block).
    */
  case class RxExtensionLoaderShutdownHook(extensionChannel: Observer[(Channel, ExtensionBlocks)],
                                           simpleBlocksChannel: Observer[(Channel, Block)]) {
    /** Shutdown Extensions and Blocks.*/
    def shutdown(): Unit = {
      extensionChannel.onComplete()
      simpleBlocksChannel.onComplete()
    }
  }

  /** Case Class for Blocks Extensions.
    * @constructor Creates a new extension block.
    * @param blocks Inputs a sequence of [[Block]].
    */
  case class ExtensionBlocks(blocks: Seq[Block]) {
    override def toString: String = s"ExtensionBlocks(${formatSignatures(blocks.map(_.uniqueId))}"
  }

  /** Case Class for State definitions.
    * @param loaderState Inputs a LoaderState.
    * @param applierState Inputs a ApplierState.
    */
  case class State(loaderState: LoaderState, applierState: ApplierState) {
    /** Gets a State from a [[LoaderState]].
      * @param newLoaderState Inputs the LoaderState.
      * @return Returns the State.
      */
    def withLoaderState(newLoaderState: LoaderState): State = {
      loaderState match {
        case wp: WithPeer => wp.timeout.cancel()
        case _ =>
      }
      State(newLoaderState, applierState)
    }

    /** Gets a State from a Idle Loader.
      * @return Returns the State.
      */
    def withIdleLoader: State = withLoaderState(LoaderState.Idle)
  }

  /** Defines a ApplierState interface. */
  sealed trait ApplierState

  /** Creates the ApplierState object for static calls. */
  object ApplierState {

    /** Case object for an Idle State.*/
    case object Idle extends ApplierState

    /** Case classe for buffer.
      * @constructor Creates a Buffer for Netty Channel and Extension Blocks.
      * @param ch The input Channel.
      * @param ext The Extension Blocks.
      */
    case class Buffer(ch: Channel, ext: ExtensionBlocks) {

      override def toString: String = s"Buffer($ext from ${id(ch)})"
    }

    /** Case Class for Applying State.
      * @param buf Inputs an Option for Buffer.
      * @param applying The Extension Blocks for Applying.
      */
    case class Applying(buf: Option[Buffer], applying: ExtensionBlocks) extends ApplierState

  }

}