package io.lunes.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import io.lunes.crypto
import io.lunes.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import io.lunes.transaction.TransactionParser._
import io.lunes.transaction.{ValidationError, _}

import scala.util.{Failure, Success, Try}

/**  Case Class for a BurnTransaction.
  * @constructor Creates a new BurnTransaction.
  * @param sender The Public Key for the Account.
  * @param assetId The input Asset ID.
  * @param amount The amount of the Transaction.
  * @param fee The Fee.
  * @param timestamp The Timestamp.
  * @param signature The Account Signature.
  */
case class BurnTransaction private(sender: PublicKeyAccount,
                                   assetId: ByteStr,
                                   amount: Long,
                                   fee: Long,
                                   timestamp: Long,
                                   signature: ByteStr)
  extends SignedTransaction with FastHashId {

  override val transactionType: TransactionType.Value = TransactionType.BurnTransaction

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(transactionType.id.toByte),
    sender.publicKey,
    assetId.arr,
    Longs.toByteArray(amount),
    Longs.toByteArray(fee),
    Longs.toByteArray(timestamp)))

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "assetId" -> assetId.base58,
      "amount" -> amount,
      "fee" -> fee
    )
  }

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(bodyBytes(), signature.arr))

}

/** The BurnTransaction Companion object.*/
object BurnTransaction {
  /** Parses input Raw data and transforms it into a BurnTransaction.
    * @param bytes The input Raw data.
    * @return Returns a BurnTransaction.
    */
  def parseBytes(bytes: Array[Byte]): Try[BurnTransaction] = Try {
    require(bytes.head == TransactionType.BurnTransaction.id)
    parseTail(bytes.tail).get
  }

  /** Parses input Raw data and transforms it into a BurnTransaction from a complex list.
    * @param bytes The input Raw data.
    * @return Returns a BurnTransaction.
    */
  def parseTail(bytes: Array[Byte]): Try[BurnTransaction] = Try {
    val sender = PublicKeyAccount(bytes.slice(0, KeyLength))
    val assetId = ByteStr(bytes.slice(KeyLength, KeyLength + AssetIdLength))
    val quantityStart = KeyLength + AssetIdLength

    val quantity = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val fee = Longs.fromByteArray(bytes.slice(quantityStart + 8, quantityStart + 16))
    val timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 16, quantityStart + 24))
    val signature = ByteStr(bytes.slice(quantityStart + 24, quantityStart + 24 + SignatureLength))
    BurnTransaction
      .create(sender, assetId, quantity, fee, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  /** Factory method for BurnTransaction objects.
	  * @constructor Creates a new BurnTransaction.
    * @param sender The Public Key for the Account.
    * @param assetId The Asset ID.
    * @param quantity The Quantity of the Transaction.
    * @param fee The Fee for the transaction.
    * @param timestamp The Timestamp.
    * @param signature The Account Signature.
    * @return Returns Either a BurnTransaction (case Success) or a ValidationError (case Failure).
    */
  def create(sender: PublicKeyAccount,
             assetId: ByteStr,
             quantity: Long,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, BurnTransaction] =
    if (quantity < 0) {
      Left(ValidationError.NegativeAmount(quantity, "assets"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(BurnTransaction(sender, assetId, quantity, fee, timestamp, signature))
    }

  /** Alternative Factory method for BurnTransaction objects.
    * @constructor Creates a new BurnTransaction without the Signature.
    * @param sender The Public Key for the Account.
    * @param assetId The Asset ID.
    * @param quantity The Quantity of the Transaction.
    * @param fee The Fee for the transaction.
    * @param timestamp The Timestamp.
    * @return Returns Either a BurnTransaction (case Success) or a ValidationError (case Failure).
    */
  def create(sender: PrivateKeyAccount,
             assetId: ByteStr,
             quantity: Long,
             fee: Long,
             timestamp: Long): Either[ValidationError, BurnTransaction] =
    create(sender, assetId, quantity, fee, timestamp, ByteStr.empty).right.map { unverified =>
      unverified.copy(signature = ByteStr(crypto.sign(sender, unverified.bodyBytes())))
    }
}
