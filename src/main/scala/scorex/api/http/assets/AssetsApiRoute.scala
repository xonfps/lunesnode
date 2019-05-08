package scorex.api.http.assets

import akka.http.scaladsl.server.Route
import com.google.common.base.Charsets
import io.lunes.settings.RestAPISettings
import io.lunes.state.diffs.CommonValidation
import io.lunes.state.{Blockchain, ByteStr}
import io.lunes.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json._
import scorex.BroadcastRoute
import scorex.account.Address
import scorex.api.http._
import io.lunes.utils.Base58
import io.lunes.transaction.assets.IssueTransaction
import io.lunes.transaction.assets.exchange.Order
import io.lunes.transaction.assets.exchange.OrderJson._
import io.lunes.transaction.smart.script.ScriptCompiler
import io.lunes.transaction.{
  AssetIdStringLength,
  TransactionFactory,
  ValidationError
}
import io.lunes.transaction.ValidationError.GenericError
//import java.io

import scorex.utils.Time
import scorex.wallet.Wallet

import scala.concurrent.Future
import scala.util.{Failure, Success}

@Path("/assets")
@Api(value = "assets")
case class AssetsApiRoute(settings: RestAPISettings,
                          wallet: Wallet,
                          utx: UtxPool,
                          allChannels: ChannelGroup,
                          blockchain: Blockchain,
                          time: Time)
    extends ApiRoute
    with BroadcastRoute {
  val MaxAddressesPerRequest = 1000

  override lazy val route =
    pathPrefix("assets") {
      balance ~ balances ~ issue ~ reissue ~ burnRoute ~ transfer ~ massTransfer ~ signOrder ~ balanceDistribution ~ details
    }
  @Path("/balance/{address}/{assetId}")
  @ApiOperation(value = "Asset's balance",
                notes = "Account's balance by given asset",
                httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address",
                           value = "Address",
                           required = true,
                           dataType = "string",
                           paramType = "path"),
      new ApiImplicitParam(name = "assetId",
                           value = "Asset ID",
                           required = true,
                           dataType = "string",
                           paramType = "path")
    ))
  def balance: Route =
    (get & path("balance" / Segment / Segment)) { (address, assetId) =>
      complete(balanceJson(address, assetId))
    }

  @Path("/{assetId}/distribution")
  @ApiOperation(value = "Asset balance distribution",
                notes = "Asset balance distribution by account",
                httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "assetId",
                           value = "Asset ID",
                           required = true,
                           dataType = "string",
                           paramType = "path")
    ))
  def balanceDistribution: Route =
    (get & path(Segment / "distribution")) { assetId =>
      complete {
        Success(assetId)
          .filter(_.length <= AssetIdStringLength)
          .flatMap(Base58.decode) match {
          case Success(byteArray) =>
            Json.toJson(blockchain.assetDistribution(ByteStr(byteArray)).map {
              case (a, b) => a.stringRepr -> b
            })
          case Failure(_) =>
            ApiError.fromValidationError(
              GenericError("Must be base58-encoded assetId"))
        }
      }
    }

  @Path("/balance/{address}")
  @ApiOperation(value = "Account's balance",
                notes = "Account's balances for all assets",
                httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address",
                           value = "Address",
                           required = true,
                           dataType = "string",
                           paramType = "path")
    ))
  def balances: Route =
    (get & path("balance" / Segment)) { address =>
      complete(fullAccountAssetsInfo(address))
    }

  @Path("/details/{assetId}")
  @ApiOperation(value = "Information about an asset",
                notes = "Provides detailed information about given asset",
                httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "assetId",
                           value = "ID of the asset",
                           required = true,
                           dataType = "string",
                           paramType = "path")
    ))
  def details: Route =
    (get & path("details" / Segment)) { id =>
      complete(assetDetails(id))
    }

  @Path("/transfer")
  @ApiOperation(value = "Transfer asset",
                notes = "Transfer asset to new address",
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
        dataType = "scorex.api.http.assets.TransferV2Request",
        defaultValue =
          "{\"sender\":\"3Mn6xomsZZepJj1GL1QaW6CaCJAq8B3oPef\",\"recipient\":\"3Mciuup51AxRrpSz7XhutnQYTkNT9691HAk\",\"assetId\":null,\"amount\":5813874260609385500,\"feeAssetId\":\"3Z7T9SwMbcBuZgcn3mGu7MMp619CTgSWBT7wvEkPwYXGnoYzLeTyh3EqZu1ibUhbUHAsGK5tdv9vJL9pk4fzv9Gc\",\"fee\":1579331567487095949,\"timestamp\":4231642878298810008}"
      )
    ))
  def transfer: Route =
    processRequest[TransferRequests](
      "transfer", { req =>
        req.eliminate(
          x => doBroadcast(TransactionFactory.transferAssetV1(x, wallet, time)),
          _.eliminate(
            x =>
              doBroadcast(TransactionFactory.transferAssetV2(x, wallet, time)),
            _ =>
              Future.successful(
                WrongJson(Some(new IllegalArgumentException(
                  "Doesn't know how to process request"))))
          )
        )
      }
    )

  @Path("/masstransfer")
  @ApiOperation(value = "Mass Transfer",
                notes = "Mass transfer of assets",
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
        dataType = "scorex.api.http.assets.MassTransferRequest",
        defaultValue =
          "{\"version\": 1, \"sender\":\"3Mn6xomsZZepJj1GL1QaW6CaCJAq8B3oPef\",\"transfers\":(\"3Mciuup51AxRrpSz7XhutnQYTkNT9691HAk\",100000000),\"fee\":100000,\"timestamp\":1517315595291}"
      )
    ))
  def massTransfer: Route =
    processRequest(
      "masstransfer",
      (t: MassTransferRequest) =>
        doBroadcast(TransactionFactory.massTransferAsset(t, wallet, time)))

  @Path("/issue")
  @ApiOperation(value = "Issue Asset",
                notes = "Issue new Asset",
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
        dataType = "scorex.api.http.assets.IssueV1Request",
        defaultValue =
          "{\"sender\":\"string\",\"name\":\"str\",\"description\":\"string\",\"quantity\":100000,\"decimals\":7,\"reissuable\":false,\"fee\":100000000}"
      )
    ))
  def issue: Route =
    processRequest(
      "issue",
      (r: IssueV1Request) =>
        doBroadcast(TransactionFactory.issueAssetV1(r, wallet, time)))

  @Path("/reissue")
  @ApiOperation(value = "Issue Asset",
                notes = "Reissue Asset",
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
        dataType = "scorex.api.http.assets.ReissueV1Request",
        defaultValue =
          "{\"sender\":\"string\",\"assetId\":\"Base58\",\"quantity\":100000,\"reissuable\":false,\"fee\":1}"
      )
    ))
  def reissue: Route =
    processRequest(
      "reissue",
      (r: ReissueV1Request) =>
        doBroadcast(TransactionFactory.reissueAssetV1(r, wallet, time)))

  @Path("/burn")
  @ApiOperation(value = "Burn Asset",
                notes = "Burn some of your assets",
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
        dataType = "scorex.api.http.assets.BurnV1Request",
        defaultValue =
          "{\"sender\":\"string\",\"assetId\":\"Base58\",\"quantity\":100,\"fee\":100000}"
      )
    ))
  def burnRoute: Route =
    processRequest(
      "burn",
      (b: BurnV1Request) =>
        doBroadcast(TransactionFactory.burnAssetV1(b, wallet, time)))

  @Path("/order")
  @ApiOperation(value = "Sign Order",
                notes = "Create order signed by address from wallet",
                httpMethod = "POST",
                produces = "application/json",
                consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Order Json with data",
        required = true,
        paramType = "body",
        dataType = "io.lunes.transaction.assets.exchange.Order"
      )
    ))
  def signOrder: Route =
    processRequest("order", (order: Order) => {
      wallet
        .privateKeyAccount(order.senderPublicKey)
        .map(pk => Order.sign(order, pk))
    })

  /**
    * Get a Json with Balance for Refered AssetID
    * @param address
    * @param assetIdStr
    * @return
    */
  private def balanceJson(address: String,
                          assetIdStr: String): Either[ApiError, JsObject] = {
    ByteStr.decodeBase58(assetIdStr) match {
      case Success(assetId) =>
        (for {
          acc <- Address.fromString(address)
        } yield
          Json.obj(
            "address" -> acc.address,
            "assetId" -> assetIdStr,
            "balance" -> JsNumber(
              BigDecimal(
                blockchain.portfolio(acc).assets.getOrElse(assetId, 0L)))
          )).left
          .map(ApiError.fromValidationError)
      case _ => Left(InvalidAddress)
    }
  }

  /**
    * Search for Lunes Asset Id
    * @param address
    * @return
    */
  private def LunesAssetId(address: String): Either[ApiError, String] = {
    val account = Address.fromString(address) match {
      case Right(value) => {
        import scala.util.control._
        val loop = Breaks
        val assetKeyList = blockchain.portfolio(value).assets.keySet.toList
        var foundLunes = false
        var foundKey: ByteStr = ByteStr(Array.emptyByteArray)
        loop.breakable {
          for (key <- assetKeyList) {
            val assetName = (blockchain
              .assetDescription(key)
              .get)
              .name
              .map(_.toChar)
              .mkString
              .toUpperCase
            if (assetName == "LUNES") {
              foundLunes = true
              foundKey = key
              loop.break;
            }
          }
        }
        if (foundLunes) {
          Right((foundKey.arr.map(_.toChar)).mkString)
        } else {
          Left(ValidationError.InsufficientLunesInStake)
        }
      }
      case Left(value) => Left(ValidationError.InsufficientLunesInStake)
    }
    account match {
      case Right(value) => Right(value)
      case Left(value) =>
        Left(InsufficientLunesInStake("Account does not have LUNES in Stake"))
    }
  }

  /**
    * Check if issuer has enough Lunes in it wallet
    * @param address
    * @return
    */
  def hasEnoughLunesInStake(address: String, assetId: String): Boolean = {

    /**
      * Check if the issuer has enough lunes in its wallet
      * Check if the address has enough lunes int its wallet
      */
    val lunesId = LunesAssetId(address) match {
      case Right(x) => x
      case Left(_)  => ""
    }

    val issuerLunesBalance = getIssuerBalance(assetId, lunesId).toLong

    val maybeAddressLunesBalance = balanceJson(address, lunesId) match {
      case Right(x) => Some((x \ "balance").as[String])
      case Left(_)  => None
    }

    val addressLunesBalance = maybeAddressLunesBalance match {
      case Some(x) => x.toLong
      case None    => 0L
    }

    val lunesMinimumFee = 20000L

    (issuerLunesBalance >= lunesMinimumFee) || (addressLunesBalance >= lunesMinimumFee)
  }

  /**
    * Get the Issuer Balance for some Asset
    * In error, return "0" balance.
    * @param assetId
    * @param lunesId
    * @return
    */
  private def getIssuerBalance(assetId: String,
                               checkAssetId: String): String = {

    val issuer = issuerForAsset(assetId)

    val maybeIssuerBalance = balanceJson(issuer, checkAssetId) match {
      case Right(x) => Some((x \ "balance").as[String])
      case Left(_)  => None
    }

    maybeIssuerBalance match {
      case Some(x) => x
      case None    => "0"
    }
  }

  /**
    * Get Assets Information for all assets for address.
    * @param address
    * @return
    */
  private def fullAccountAssetsInfo(
      address: String): Either[ApiError, JsObject] =
    (for {
      acc <- Address.fromString(address)
    } yield {
      Json.obj(
        "address" -> acc.address,
        "balances" -> JsArray(
          (for {
            (assetId, balance) <- blockchain.portfolio(acc).assets
            if balance > 0
            assetInfo <- blockchain.assetDescription(assetId)
            (_, (issueTransaction: IssueTransaction)) <- blockchain
              .transactionInfo(assetId)
            sponsorBalance = if (assetInfo.sponsorship != 0) {
              Some(
                blockchain.portfolio(issueTransaction.sender).spendableBalance)
            } else {
              None
            }
          } yield
            Json.obj(
              "assetId" -> assetId.base58,
              "balance" -> balance,
              "reissuable" -> assetInfo.reissuable,
              "minSponsoredAssetFee" -> (assetInfo.sponsorship match {
                case 0           => JsNull
                case sponsorship => JsNumber(sponsorship)
              }),
              "sponsorBalance" -> sponsorBalance,
              "quantity" -> JsNumber(BigDecimal(assetInfo.totalVolume)),
              "issueTransaction" -> issueTransaction.json()
            )).toSeq)
      )
    }).left.map(ApiError.fromValidationError)

  /**
    * Asset Details in the Blockchain
    * @param assetId
    * @return
    */
  private def assetDetails(assetId: String): Either[ApiError, JsObject] =
    (for {
      id <- ByteStr.decodeBase58(assetId).toOption.toRight("Incorrect asset ID")
      tt <- blockchain
        .transactionInfo(id)
        .toRight("Failed to find issue transaction by ID")
      (h, mtx) = tt
      tx <- (mtx match {
        case t: IssueTransaction => Some(t)
        case _                   => None
      }).toRight("No issue transaction found with given asset ID")
      description <- blockchain
        .assetDescription(id)
        .toRight("Failed to get description of the asset")
      complexity <- description.script.fold[Either[String, Long]](Right(0))(
        ScriptCompiler.estimate)
    } yield {
      JsObject(
        Seq(
          "assetId" -> JsString(id.base58),
          "issueHeight" -> JsNumber(h),
          "issueTimestamp" -> JsNumber(tx.timestamp),
          "issuer" -> JsString(tx.sender.toString),
          "name" -> JsString(new String(tx.name, Charsets.UTF_8)), //asset name
          "description" -> JsString(new String(tx.description, Charsets.UTF_8)),
          "decimals" -> JsNumber(tx.decimals.toInt),
          "reissuable" -> JsBoolean(description.reissuable),
          "quantity" -> JsNumber(BigDecimal(description.totalVolume)),
          "script" -> Json.toJson(description.script.map(_.bytes().base58)),
          "scriptText" -> Json.toJson(description.script.map(_.text)),
          "complexity" -> JsNumber(complexity),
          "extraFee" -> JsNumber(
            if (description.script.isEmpty) 0
            else CommonValidation.ScriptExtraFee),
          "minSponsoredAssetFee" -> (description.sponsorship match {
            case 0           => JsNull
            case sponsorship => JsNumber(sponsorship)
          })
        )
      )
    }).left.map(m => CustomValidationError(m))

  /**
    * Inner method for acquire Issuer for an Asset
    * @param assetId Asset String ID
    */
  private def issuerForAsset(assetId: String): String = {
    val assetDt = assetDetails(assetId) match {
      case Left(x) => None

      case Right(x) => Some(x.value("issuer").toString())
    }
    val issuer = assetDt match {
      case None        => ""
      case Some(value) => value
    }
    issuer
  }

//  @Path("/sponsor")
//  @ApiOperation(value = "Sponsor an Asset", httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "scorex.api.http.assets.SponsorFeeRequest",
        defaultValue =
          "{\"sender\":\"string\",\"assetId\":\"Base58\",\"minSponsoredAssetFee\":100000000,\"fee\":100000000}"
      )
    ))
  def sponsorRoute: Route =
    processRequest(
      "sponsor",
      (req: SponsorFeeRequest) => {
        val checkLunesStakeRule = hasEnoughLunesInStake(req.sender, req.assetId)
        doBroadcast(
          TransactionFactory.sponsor(req, wallet, time, checkLunesStakeRule))
      }
    )
}
