package scorex.api.http.alias

import akka.http.scaladsl.server.Route
import io.lunes.settings.RestAPISettings
import io.lunes.state.Blockchain
import io.lunes.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.{Format, Json}
import scorex.BroadcastRoute
import scorex.account.Alias
import scorex.api.http._
import io.lunes.transaction._
import scorex.utils.Time
import scorex.wallet.Wallet

@Path("/addresses/alias")
@Api(value = "addresses")
case class AliasApiRoute(settings: RestAPISettings,
                         wallet: Wallet,
                         utx: UtxPool,
                         allChannels: ChannelGroup,
                         time: Time,
                         blockchain: Blockchain)
    extends ApiRoute
    with BroadcastRoute {

  override val route = pathPrefix("addresses" / "alias") {
    alias ~ addressOfAlias ~ aliasOfAddress
  }

  @Path("/alias-create")
  @ApiOperation(value = "Creates an alias",
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
        dataType = "scorex.api.http.alias.CreateAliasV1Request",
        defaultValue =
          "{\n\t\"alias\": \"aliasalias\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000\n}"
      )
    ))
  @ApiResponses(
    Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def alias: Route =
    processRequest("create",
                   (t: CreateAliasV1Request) =>
                     doBroadcast(TransactionFactory.aliasV1(t, wallet, time)))
  @Path("/by-alias/{alias}")
  @ApiOperation(
    value = "Account",
    notes =
      "Returns an address associated with an Alias. Alias should be plain text without an 'alias' prefix and network code.",
    httpMethod = "GET"
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "alias",
                           value = "Alias",
                           required = true,
                           dataType = "string",
                           paramType = "path")
    ))
  def addressOfAlias: Route = (get & path("by-alias" / Segment)) { aliasName =>
    val result = Alias.buildWithCurrentNetworkByte(aliasName) match {
      case Right(alias) =>
        blockchain.resolveAlias(alias) match {
          case Right(addr) => Right(Address(addr.stringRepr))
          case _           => Left(AliasDoesNotExist(alias))
        }
      case Left(err) => Left(ApiError.fromValidationError(err))
    }
    complete(result)
  }

  @Path("/by-address/{address}")
  @ApiOperation(value = "Alias",
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
  def aliasOfAddress: Route = (get & path("by-address" / Segment)) {
    addressString =>
      val result: Either[ApiError, Seq[String]] = scorex.account.Address
        .fromString(addressString)
        .map(acc => blockchain.aliasesOfAddress(acc).map(_.stringRepr))
        .left
        .map(ApiError.fromValidationError)

      val jsonResult: Either[ApiError, AliasTree] = result match {
        case Left(x) => Left(x)
        case Right(x) => {
          val stringResults = x.toList

          val stringLists = stringResults.map(x => {
            val Array(a, b, c) = x.split(":")
            List(a, b, c)
          })

          val callEntities =
            (for (List(a, b, c) <- stringLists if (a == "alias"))
              yield
                (b, for (List(a, b, c) <- stringLists) yield c)).head //todo: fix it to unique

          val (netCode, aliasesList) = callEntities

          Right(AliasTree(aliasesList))
        }
      }

      complete(jsonResult)
  }

  case class Address(address: String)

  case class NetAliases(netCode: Int, Aliases: List[String])

  case class AliasTree(alias: List[String])

  implicit val addressFormat: Format[Address] = Json.format

  implicit val aliasTreeFormat: Format[AliasTree] = Json.format

}
