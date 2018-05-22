package io.lunes.transaction

import io.lunes.state2.{ByteStr, StateReader, StateStorage}
import scorex.account.{Address, AddressScheme, Alias}
import scorex.api.http.{AliasNotExists, ApiError}
import com.google.common.primitives.Bytes


import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import io.lunes.utx.UtxPool
import io.lunes.settings.RestAPISettings
import io.lunes.state2.StateReader
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import play.api.libs.json.{Format, Json}
import scorex.BroadcastRoute
import scorex.account.Alias
import scorex.api.http._
import io.lunes.transaction._
import scorex.utils.Time
import scorex.wallet.Wallet


case class LunesAliasUserdata(val state: StateReader, val storage: StateStorage) {

  def addressOfAlias(userdata:Array[Byte]) : Address =  {
    val result = Alias.buildWithCurrentNetworkByte(ByteStr(userdata).toString) match {
      case Right(alias) =>
        state().resolveAlias(alias) match {
          case Some(addr) => addr
          case None => {
            // input the new alias
            val subPrefix = "state".getBytes
            val Separator: Array[Byte] = Array[Byte](':')
            def schemeByte: Byte = AddressScheme.current.chainId

            def makeKey(prefix: Array[Byte], key: Array[Byte]): Array[Byte] = Bytes.concat(subPrefix, Separator, prefix, Separator, key)
            val AliasToAddressPrefix = "alias-address".getBytes

            val aliasKey = makeKey(AliasToAddressPrefix, alias.bytes.arr)
              storage.put(aliasKey, alias.bytes.arr, None)

          }
        }
      case Left(err) => Left(ApiError.fromValidationError(err))
    }
    complete(result)
  }

}
