package io.lunes.database

import java.util

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import io.lunes.state2._
import scorex.account.Address
import scorex.block.Block

import scala.collection.JavaConverters._

trait Caches extends Blockchain {
  import Caches._

  protected def maxCacheSize: Int

  @volatile
  private var heightCache = loadHeight()
  protected def loadHeight(): Int
  override def height: Int = heightCache

  @volatile
  private var scoreCache = loadScore()
  protected def loadScore(): BigInt
  override def score: BigInt = scoreCache

  @volatile
  private var lastBlockCache = loadLastBlock()
  protected def loadLastBlock(): Option[Block]
  override def lastBlock: Option[Block] = lastBlockCache

  private val transactionIds                                       = new util.HashMap[ByteStr, Long]()
  protected def forgetTransaction(id: ByteStr): Unit               = transactionIds.remove(id)
  override def containsTransaction(id: ByteStr): Boolean           = transactionIds.containsKey(id)
  override def learnTransactions(values: Map[ByteStr, Long]): Unit = transactionIds.putAll(values.asJava)
  override def forgetTransactions(pred: (ByteStr, Long) => Boolean): Map[ByteStr, Long] = {
    val removedTransactions = Map.newBuilder[ByteStr, Long]
    val iterator            = transactionIds.entrySet().iterator()
    while (iterator.hasNext) {
      val e = iterator.next()
      if (pred(e.getKey, e.getValue)) {
        removedTransactions += e.getKey -> e.getValue
        iterator.remove()
      }
    }
    removedTransactions.result()
  }

  protected def loadMaxAddressId(): BigInt

  private val addressIdCache: LoadingCache[Address, Option[BigInt]] = cache(maxCacheSize, loadAddressId)
  protected def loadAddressId(address: Address): Option[BigInt]
  protected def addressId(address: Address): Option[BigInt] = addressIdCache.get(address)

  @volatile
  protected var approvedFeaturesCache: Map[Short, Int] = loadApprovedFeatures()
  protected def loadApprovedFeatures(): Map[Short, Int]
  override def approvedFeatures: Map[Short, Int] = approvedFeaturesCache

  @volatile
  protected var activatedFeaturesCache: Map[Short, Int] = loadActivatedFeatures()
  protected def loadActivatedFeatures(): Map[Short, Int]
  override def activatedFeatures: Map[Short, Int] = activatedFeaturesCache
}

object Caches {
  def cache[K <: AnyRef, V <: AnyRef](maximumSize: Int, loader: K => V): LoadingCache[K, V] =
    CacheBuilder
      .newBuilder()
      .maximumSize(maximumSize)
      .build(new CacheLoader[K, V] {
        override def load(key: K) = loader(key)
      })
}