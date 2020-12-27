package scorex.api.http.truth

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.{Truth, PublicKeyAccount}
import scorex.api.http.BroadcastRequest
import io.lunes.transaction.{
  CreateTruthTransaction,
  CreateTruthTransactionV2,
  Proofs,
  ValidationError
}
import cats.implicits._

case class SignedCreateTruthV2Request(
    @ApiModelProperty(required = true)
    version: Byte,
    @ApiModelProperty(value = "Base58 encoded sender public key",
                      required = true)
    senderPublicKey: String,
    @ApiModelProperty(required = true)
    fee: Long,
    @ApiModelProperty(value = "Truth", required = true)
    truth: String,
    @ApiModelProperty(required = true)
    timestamp: Long,
    @ApiModelProperty(required = true)
    proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, CreateTruthTransaction] =
    for {
      _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s =>
        parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs <- Proofs.create(_proofBytes)
      _truth <- Truth.buildWithCurrentNetworkByte(truth)
      _t <- CreateTruthTransactionV2.create(version,
                                            _sender,
                                            _truth,
                                            fee,
                                            timestamp,
                                            _proofs)
    } yield _t
}

object SignedCreateTruthV2Request {
  implicit val broadcastTruthV2RequestReadsFormat
    : Format[SignedCreateTruthV2Request] = Json.format
}
