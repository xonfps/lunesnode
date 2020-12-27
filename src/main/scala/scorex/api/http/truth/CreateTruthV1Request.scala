package scorex.api.http.truth

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class CreateTruthV1Request(
    @ApiModelProperty(value = "Base58 encoded sender public key",
                      required = true)
    sender: String,
    @ApiModelProperty(value = "Truth", required = true)
    truth: String,
    @ApiModelProperty(required = true)
    fee: Long,
    timestamp: Option[Long] = None)

object CreateTruthV1Request {
  implicit val aliasV1RequestFormat: Format[CreateTruthV1Request] = Json.format
}
