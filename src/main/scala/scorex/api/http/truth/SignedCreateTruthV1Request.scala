package scorex.api.http.truth

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.{Truth, PublicKeyAccount}
import scorex.api.http.BroadcastRequest
import io.lunes.transaction.TransactionParsers.SignatureStringLength
import io.lunes.transaction.{CreateTruthTransactionV1, ValidationError}

case class SignedCreateTruthV1Request(
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
    signature: String)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, CreateTruthTransactionV1] =
    for {
      _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature,
                                "invalid.signature",
                                SignatureStringLength)
      _truth <- Truth.buildWithCurrentNetworkByte(truth)
      _t <- CreateTruthTransactionV1.create(_sender,
                                            _truth,
                                            fee,
                                            timestamp,
                                            _signature)
    } yield _t
}

object SignedCreateTruthV1Request {
  implicit val broadcastTruthV1RequestReadsFormat
    : Format[SignedCreateTruthV1Request] = Json.format
}
