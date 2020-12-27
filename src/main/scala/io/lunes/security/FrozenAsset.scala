package io.lunes.security

import io.lunes.transaction.AssetId

/**
  * Frozen Asset on some Account.
  * @param account BanAddress element for the Account.
  * @param asset Hash for the asset to be frozen.
  */
case class FrozenAsset(account: BanAddress, asset: AssetId) {

  def checksWith(accountId: BanAddress, assetId: AssetId) =
    account.checksWith(accountId.account) && assetId == asset

  def checksWith(accountId: String, assetId: AssetId) =
    account.checksWith(accountId) && assetId == asset

  def checksWith(accountId: String, assetId: String) =
    account.checksWith(accountId) && asset == assetId.toCharArray.map(_.toByte)
}

/**
  *
  * @param list
  */
class FrozenAssetList(list: List[FrozenAsset]) {

  /**
    * Frozen Assets for given account
    * @param input
    * @return
    */
  def frozenAssetsOf(input: String): List[AssetId] =
    list.filter(_.account.checksWith(input)).collect {
      case a: FrozenAsset => a.asset
    }

  def unfreeze(account: String): FrozenAssetList =
    FrozenAssetList(list.filter(!_.account.checksWith(account)))

  def unfreeze(account: BanAddress, assetId: AssetId): FrozenAssetList =
    FrozenAssetList(list.filter(!_.checksWith(account, assetId)))

  def unfreeze(account: String, assetId: AssetId): FrozenAssetList =
    FrozenAssetList(list.filter(!_.checksWith(account, assetId)))

  def checksWith(accountId: String, assetId: String): Boolean =
    list.filter(_.checksWith(accountId, assetId)).nonEmpty
}

/**
  *
  */
object FrozenAssetList {

  def apply(input: List[FrozenAsset]) = new FrozenAssetList(input)

  def allAccountsForAsset(accounts: List[String], asset: AssetId) =
    new FrozenAssetList(
      accounts.map(acc => FrozenAsset(BanAddress(acc), asset)))
}
