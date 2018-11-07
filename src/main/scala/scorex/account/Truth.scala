package scorex.account

import io.lunes.state.ByteStr
import scorex.serialization.Deser
import io.lunes.transaction.ValidationError
import io.lunes.transaction.ValidationError.GenericError

sealed trait Truth extends AddressOrAlias {
  lazy val stringRepr: String = Alias.Prefix + networkByte.toChar + ":" + name
  lazy val bytes: ByteStr = ByteStr(
    Alias.AddressVersion +: networkByte +: Deser.serializeArray(
      name.getBytes("UTF-8")))

  val name: String
  val networkByte: Byte

}


object Truth{
  val Prefix: String = "truth:"

  val AddressVersion: Byte = 2
  val MinLength = 4
  val MaxLength = 30

  val truthAlphabet = "-.0123456789@_abcdefghijklmnopqrstuvwxyz"

  private val AliasPatternInfo =
    "Alias string pattern is 'alias:<chain-id>:<address-alias>"  // todo: change this

  private def schemeByte: Byte = AddressScheme.current.chainId

  private def validAliasChar(c: Char): Boolean =
    ('0' <= c && c <= '9') || ('a' <= c && c <= 'z') || c == '_' || c == '@' || c == '-' || c == '.'

  private def buildAlias(networkByte: Byte, // todo: change this
                         name: String): Either[ValidationError, Alias] = {

    case class AliasImpl(networkByte: Byte, name: String) extends Alias

    if (name.length < MinLength || MaxLength < name.length)
      Left(
        GenericError(
          s"Alias '$name' length should be between $MinLength and $MaxLength"))
    else if (!name.forall(validAliasChar))
      Left(
        GenericError(
          s"Alias should contain only following characters: $truthAlphabet"))
    else if (networkByte != schemeByte)
      Left(GenericError("Alias network char doesn't match current scheme"))
    else
      Right(AliasImpl(networkByte, name))
  }

  def buildWithCurrentNetworkByte(
                                   name: String): Either[ValidationError, Alias] =
    buildAlias(schemeByte, name)

  def fromString(str: String): Either[ValidationError, Alias] =
    if (!str.startsWith(Prefix)) {
      Left(GenericError(AliasPatternInfo)) // todo: check this
    } else {
      val charSemicolonAlias = str.drop(Prefix.length)
      val networkByte = charSemicolonAlias(0).toByte
      val name = charSemicolonAlias.drop(2)
      if (charSemicolonAlias(1) != ':') {
        Left(GenericError(AliasPatternInfo))  //todo: check this
      } else {
        buildAlias(networkByte, name)
      }
    }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Alias] = {  // todo: check this
    bytes.headOption match {
      case Some(AddressVersion) =>
        val networkChar = bytes.tail.head
        if (networkChar != schemeByte) {
          Left(GenericError("Alias network byte doesn't match current scheme"))
        } else
          buildAlias(networkChar, new String(bytes.drop(4), "UTF-8"))
      case _ => Left(GenericError("Bad alias bytes"))
    }
  }

}