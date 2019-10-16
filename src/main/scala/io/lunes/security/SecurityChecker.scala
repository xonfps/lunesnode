package io.lunes.security

object SecurityChecker {

  var bannedAddress = BanAddressList(List.empty)

  var frozen = FrozenAssetList(List.empty)

  var frozenAssetName: List[String] = List.empty

  val essentialBanned = List("37ms8U8BDPrC24DoUrivA8Lxuu1J1gWNb79",
                             "37uDxz6BQX88fPCCEBwhY4GoCW6YWwZsAQS")
  addBannedAddress(essentialBanned(0))
  addBannedAddress(essentialBanned(1))

  val essentialAssetName = List("LUNES")
  addFrozenAssetName(essentialAssetName(0))

  // todo: implement the following list
  /*
   * adicionar os ativos banidos -- colocar dinamicamente em banned
   */
  def checkAddress(input: String) = bannedAddress.banned(input)

  def addBannedAddress(input: String) =
    if (!bannedAddress.banned(input))
      bannedAddress = BanAddressList(
        bannedAddress.list ++ BanAddressList(List(BanAddress(input))).list)

  def addFrozenAssetName(input: String) =
    if (frozenAssetName.filter(_ == input).isEmpty)
      frozenAssetName = frozenAssetName ++ List(input)

  def checkFrozenAsset(account: String, assetId: String): Boolean =
    frozen.checksWith(account, assetId)

}
