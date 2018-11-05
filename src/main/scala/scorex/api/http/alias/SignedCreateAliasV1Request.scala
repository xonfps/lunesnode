package scorex.api.http.alias

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.{Alias, PublicKeyAccount}
import scorex.api.http.BroadcastRequest
import io.lunes.transaction.TransactionParsers.SignatureStringLength
import io.lunes.transaction.{CreateAliasTransactionV1, ValidationError}

case class SignedCreateAliasV1Request(
    @ApiModelProperty(value = "Base58 encoded sender public key",
                      required = true)
    senderPublicKey: String,
    @ApiModelProperty(required = true)
    fee: Long,
    @ApiModelProperty(value = "Alias", required = true)
    alias: String,
    @ApiModelProperty(required = true)
    timestamp: Long,
    @ApiModelProperty(required = true)
    signature: String)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, CreateAliasTransactionV1] =
    for {
      _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature,
                                "invalid.signature",
                                SignatureStringLength)
      _alias <- Alias.buildWithCurrentNetworkByte(alias)
      _t <- CreateAliasTransactionV1.create(_sender,
                                            _alias,
                                            fee,
                                            timestamp,
                                            _signature)
    } yield _t
}

object SignedCreateAliasV1Request {
  implicit val broadcastAliasV1RequestReadsFormat
    : Format[SignedCreateAliasV1Request] = Json.format
}
