package io.lunes.transaction

import io.lunes.state2.{ByteStr, StateReader, StateStorage}
import scorex.account.{Address, AddressScheme, Alias}
import com.google.common.primitives.Bytes


case class LunesAliasUserdata(val state: StateReader, val storage: StateStorage) {

  def addressOfAlias(userdata:Array[Byte]) : Option[Address] =  {
    Alias.buildWithCurrentNetworkByte(ByteStr(userdata).toString) match {
      case Right(alias) =>
        state().resolveAlias(alias) match {
          case Some(addr) => {  // Found the address
            Option(addr)  // returning object
          }
          case None => { // Address not found
            // input the new alias
            val subPrefix = "state".getBytes
            val Separator: Array[Byte] = Array[Byte](':')
            def schemeByte: Byte = AddressScheme.current.chainId

            def makeKey(prefix: Array[Byte], key: Array[Byte]): Array[Byte] = Bytes.concat(subPrefix, Separator, prefix, Separator, key)
            val AliasToAddressPrefix = "alias-address".getBytes

            val aliasKey = makeKey(AliasToAddressPrefix, alias.bytes.arr)
              storage.put(aliasKey, alias.bytes.arr, None)
            Option(Address.fromBytes(alias.bytes.arr).right)  // returning object
          }
        }
      case Left(err) => {
        throw new Exception(err.toString)
      }
    }
    None
  }
}
