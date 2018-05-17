package io.lunes.transaction

import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58

/** Trait for Proven Transactions.*/
trait ProvenTransaction extends Transaction with Proven {
  /** Gets a Tuple for Proof Field based on String an JSON Wrapper.
    * @return The Tuple.
    */
  protected def proofField:(String, Json.JsValueWrapper) = "proofs" -> this.proofs.proofs.map(_.base58)

  val bodyBytes: Coeval[Array[Byte]]

  /** Returns a JSON object based on the Transaction.
    * @return The JSON object.
    */
  protected def jsonBase(): JsObject = Json.obj("type" -> transactionType.id,
    "id" -> id().base58,
    "sender" -> sender.address,
    "senderPublicKey" -> Base58.encode(sender.publicKey),
    "fee" -> assetFee._2,
    "timestamp" -> timestamp) ++ Json.obj(proofField)
}
