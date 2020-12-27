package scorex.api.http.truth

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class CreateTruthV2Request(
    @ApiModelProperty(required = true)
    version: Byte,
    @ApiModelProperty(value = "Base58 encoded sender public key",
                      required = true)
    sender: String,
    @ApiModelProperty(value = "Truth", required = true)
    truth: String,
    @ApiModelProperty(required = true)
    fee: Long,
    timestamp: Option[Long] = None)

object CreateTruthV2Request {
  implicit val aliasV2RequestFormat: Format[CreateTruthV2Request] = Json.format
}
