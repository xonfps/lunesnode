package io.lunes.transaction

import scorex.account.Address

/** Asset Account Mapping.
  * @param account The [[Address]] for the Account.
  * @param assetId The AssetID.
  */
case class AssetAcc(account: Address, assetId: Option[AssetId]) {
  lazy val key: String = assetId match {
    case None => account.address
    case Some(id) => account.address + id.base58
  }
}

