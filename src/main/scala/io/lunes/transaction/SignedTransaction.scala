package io.lunes.transaction

import io.lunes.crypto
import io.lunes.state2.{ByteStr, _}
import monix.eval.Coeval

/** Trait for Signed Transactions. */
trait SignedTransaction extends ProvenTransaction with Signed {
  /** Gets a Tuple for Proof Field based on String an JSON Wrapper.
    * @return The Tuple.
    */
  protected override def proofField = "signature" -> this.signature.base58

  val signature: ByteStr

  /** Returns a Proof object based on a signature.
    * @return Returns a
    */
  def proofs: Proofs = Proofs.create(Seq(signature)).explicitGet()

  val signatureValid: Coeval[Boolean] = Coeval.evalOnce(crypto.verify(signature.arr, bodyBytes(), sender.publicKey))
}
