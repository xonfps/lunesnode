package io.lunes.transaction

import io.lunes.crypto
import io.lunes.state2.ByteStr
import monix.eval.Coeval

/** Trait for IDs generated from Fast Hash Algorithm.*/
trait FastHashId extends ProvenTransaction {

  val id: Coeval[AssetId] = Coeval.evalOnce(ByteStr(crypto.fastHash(bodyBytes())))
}
