package io.lunes.security

object SecurityChecker {

  val bannedAddress = BanAddressList(
    List(
      BanAddress("37ms8U8BDPrC24DoUrivA8Lxuu1J1gWNb79"),
      BanAddress("37uDxz6BQX88fPCCEBwhY4GoCW6YWwZsAQS"),
      BanAddress("37UVytB12pkXTuyNkKkZtWuzDaxc4yE2mZd")
    )
  )

  val frozen = FrozenAssetList(List.empty)

  def checkAddress(input: String) : Boolean = bannedAddress.banned(input)

  def checkFrozenAsset(account: String, assetId: String): Boolean =
    frozen.checksWith(account, assetId)

}
