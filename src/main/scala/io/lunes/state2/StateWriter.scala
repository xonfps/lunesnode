package io.lunes.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.implicits._
import io.lunes.metrics.Instrumented
import io.lunes.state2.reader.StateReaderImpl
import scorex.utils.ScorexLogging

/** Provides an interface for State Writers.  */
trait StateWriter {
  /** Applies a BlockDiff to the State.
    * @param blockDiff the input BlockDiff.
    */
  def applyBlockDiff(blockDiff: BlockDiff): Unit

  /** Clears the State.
    */
  def clear(): Unit
}

/** Implements State Writer.
  * @param p The State Storage database.
  * @param synchronizationToken The Synchronization Token.
  */
class StateWriterImpl(p: StateStorage, synchronizationToken: ReentrantReadWriteLock)
  extends StateReaderImpl(p, synchronizationToken) with StateWriter with ScorexLogging with Instrumented {
  /** Applies a BlockDiff to the State.
    * @param blockDiff The input BlockDiff.
    */
  override def applyBlockDiff(blockDiff: BlockDiff): Unit = write("applyBlockDiff") { implicit l =>
    val oldHeight = sp().getHeight
    val newHeight = oldHeight + blockDiff.heightDiff
    log.debug(s"Starting persist from $oldHeight to $newHeight")

    val b = sp().createBatch()

    measureLog("transactions")(sp().putTransactions(b, blockDiff.txsDiff))

    measureSizeLog("snapshots")(blockDiff.snapshots)(
      _.foreach { case (acc, snapshotsByHeight) =>
        snapshotsByHeight.foreach { case (h, snapshot) =>
          sp().putBalanceSnapshots(b, acc, h, (snapshot.prevHeight, snapshot.balance, snapshot.effectiveBalance))
        }
        sp().putLastBalanceSnapshotHeight(b, acc, snapshotsByHeight.keys.max)
      })

    sp().setHeight(b, newHeight)

    sp().commit(b)

    log.info(s"BlockDiff commit complete. Persisted height = $newHeight")
  }

  /** Clear the State.
    */
  override def clear(): Unit = write("clear") { implicit l =>
    val b = sp().createBatch()
    sp().removeEverything(b)
    sp().setHeight(b, 0)
    sp().commit(b)
  }
}
