package io.lunes.security

import io.lunes.state.ByteStr

/**
  * Banned Account from Transactions. This Account will be banned for specific assets only.
  * @param account Account Hash String.
  */
case class BanAddress(account: String) {

  /**
    * Convert account hash from String to ByteStr.
    * @return converted account.
    */
  def toByteStr: ByteStr = ByteStr(account.toCharArray.map(_.toByte))

  /**
    * Checks from compatibility with input hash.
    * @param input
    * @return True if they are the same.
    */
  def checksWith(input: String): Boolean = input == account
}

/**
  * List of Banned Addresses.
  * @param list A list of Banned Addresses.
  */
case class BanAddressList(list: List[BanAddress]) {

  /**
    * Checks if a hash is in the banned list.
    * @param input
    * @return True if in.
    */
  def banned(input: String): Boolean = list.filter(_.checksWith(input)).nonEmpty
}
