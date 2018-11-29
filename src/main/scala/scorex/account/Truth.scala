package scorex.account

import io.lunes.state.ByteStr
import scorex.serialization.Deser
import io.lunes.transaction.ValidationError
import io.lunes.transaction.ValidationError.GenericError

sealed trait Truth extends AddressOrAlias {
  lazy val stringRepr: String = Truth.Prefix + networkByte.toChar + ":" + name
  lazy val bytes: ByteStr = ByteStr(
    Truth.AddressVersion +: networkByte +: Deser.serializeArray(
      name.getBytes("UTF-8")))

  val name: String
  val networkByte: Byte

}

object Truth {
  val Prefix: String = "truth:"

  val AddressVersion: Byte = 2 // todo: check for change related to alias
  val MinLength = 4
  val MaxLength = 30

  private val TruthPatternInfo =
    "Truth string pattern is 'truth:<chain-id>:<address-alias>"

  private def schemeByte: Byte = AddressScheme.current.chainId

  private def buildTruth(networkByte: Byte,
                         name: String): Either[ValidationError, Truth] = {

    case class TruthImpl(networkByte: Byte, name: String) extends Truth

    if (name.length < MinLength || MaxLength < name.length)
      Left(
        GenericError(
          s"Truth '$name' length should be between $MinLength and $MaxLength"))
    else if (networkByte != schemeByte)
      Left(GenericError("Truth network char doesn't match current scheme"))
    else
      Right(TruthImpl(networkByte, name))
  }

  def buildWithCurrentNetworkByte(
      name: String): Either[ValidationError, Truth] =
    buildTruth(schemeByte, name)

  def fromString(str: String): Either[ValidationError, Truth] =
    if (!str.startsWith(Prefix)) {
      Left(GenericError(TruthPatternInfo))
    } else {
      val charSemicolonTruth = str.drop(Prefix.length)
      val networkByte = charSemicolonTruth(0).toByte
      val name = charSemicolonTruth.drop(2)
      if (charSemicolonTruth(1) != ':') {
        Left(GenericError(TruthPatternInfo))
      } else {
        buildTruth(networkByte, name)
      }
    }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Truth] = {
    bytes.headOption match {
      case Some(AddressVersion) =>
        val networkChar = bytes.tail.head
        if (networkChar != schemeByte) {
          Left(GenericError("Truth network byte doesn't match current scheme"))
        } else
          buildTruth(networkChar, new String(bytes.drop(4), "UTF-8"))
      case _ => Left(GenericError("Bad truth bytes"))
    }
  }

}
