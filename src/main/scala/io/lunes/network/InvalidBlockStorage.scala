package io.lunes.network

import com.google.common.cache.CacheBuilder
import io.lunes.network.InvalidBlockStorageImpl._
import io.lunes.state.ByteStr
import io.lunes.transaction.ValidationError

import scala.concurrent.duration.FiniteDuration

trait InvalidBlockStorage {
  def add(blockId: ByteStr, validationError: ValidationError): Unit

  def find(blockId: ByteStr): Option[ValidationError]
}

class InvalidBlockStorageImpl(settings: InvalidBlockStorageSettings)
    extends InvalidBlockStorage {
  private val cache = CacheBuilder
    .newBuilder()
    .expireAfterWrite(settings.timeout.length, settings.timeout.unit)
    .build[ByteStr, ValidationError]()

  override def add(blockId: ByteStr, validationError: ValidationError): Unit =
    cache.put(blockId, validationError)

  override def find(blockId: ByteStr): Option[ValidationError] =
    Option(cache.getIfPresent(blockId))
}

object InvalidBlockStorageImpl {

  case class InvalidBlockStorageSettings(maxSize: Int, timeout: FiniteDuration)

}
