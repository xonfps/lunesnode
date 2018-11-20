package scorex.api.http.truth

import akka.http.scaladsl.server.Route
import io.lunes.settings.RestAPISettings
import io.lunes.state.Blockchain
import io.lunes.transaction.TransactionFactory
import io.lunes.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.{Format, Json}
import scorex.BroadcastRoute
import scorex.account.Truth
import scorex.api.http.{AliasDoesNotExist, ApiError, ApiRoute}
import scorex.utils.Time
import scorex.wallet.Wallet

@Path("/addresses/truth")
@Api(value = "addresses")
case class TruthApiRoute(settings: RestAPISettings,
                         wallet: Wallet,
                         utx: UtxPool,
                         allChannels: ChannelGroup,
                         time: Time,
                         blockchain: Blockchain)
    extends ApiRoute
    with BroadcastRoute {

  override val route = pathPrefix("addresses" / "truth") {
    truth ~ addressOfTruth ~ truthOfAddress
  }

  @Path("/truth-create")
  @ApiOperation(value = "Creates an truth",
                httpMethod = "POST",
                produces = "application/json",
                consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "scorex.api.http.alias.CreateAliasV1Request", //todo: check and change
        defaultValue =
          "{\n\t\"alias\": \"aliasalias\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000\n}" //todo: check and change
      )
    ))
  @ApiResponses(
    Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def truth: Route =
    processRequest(
      "create",
      (t: CreateTruthV1Request) => // todo: check and change both this and down
        doBroadcast(TransactionFactory.truthV1(t, wallet, time)))
  @Path("/by-truth/{truth}")
  @ApiOperation(
    value = "Account",
    notes =
      "Returns an address associated with an Truth. Alias should be plain text without an 'alias' prefix and network code.",
    httpMethod = "GET"
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "truth",
                           value = "Truth",
                           required = true,
                           dataType = "string",
                           paramType = "path")
    ))
  def addressOfTruth: Route = (get & path("by-truth" / Segment)) { aliasName =>
    val result = Truth.buildWithCurrentNetworkByte(aliasName) match {
      case Right(truth) =>
        blockchain.resolveAlias(truth) match {
          case Right(addr) => Right(Address(addr.stringRepr))
          case _           => Left(AliasDoesNotExist(truth))
        }
      case Left(err) => Left(ApiError.fromValidationError(err))
    }
    complete(result)
  }

  @Path("/by-address/{address}")
  @ApiOperation(value = "Truth",
                notes =
                  "Returns a collection of aliases associated with an Address",
                httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address",
                           value = "3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7",
                           required = true,
                           dataType = "string",
                           paramType = "path")
    ))
  def truthOfAddress: Route = (get & path("by-address" / Segment)) {
    addressString =>
      val result: Either[ApiError, Seq[String]] = scorex.account.Address
        .fromString(addressString)
        .map(acc => blockchain.aliasesOfAddress(acc).map(_.stringRepr)) //todo: add truth method in blockchain
        .left
        .map(ApiError.fromValidationError)

      val jsonResult: Either[ApiError, TruthTree] = result match {
        case Left(x) => Left(x)
        case Right(x) => {
          val stringResults = x.toList

          val stringLists = stringResults.map(x => {
            val Array(a, b, c) = x.split(":")
            List(a, b, c)
          })

          val callEntities =
            (for (List(a, b, c) <- stringLists if (a == "truth"))
              yield
                (b, for (List(a, b, c) <- stringLists) yield c)).head //todo: fix it to unique

          val (netCode, aliasesList) = callEntities

          Right(TruthTree(aliasesList))
        }
      }

      complete(jsonResult)
  }

  case class Address(address: String)

  case class NetTruths(netCode: Int, Truths: List[String])

  case class TruthTree(truth: List[String])

  implicit val addressFormat: Format[Address] = Json.format

  implicit val aliasTreeFormat: Format[TruthTree] = Json.format

}
