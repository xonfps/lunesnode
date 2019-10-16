package io.lunes.security

import io.lunes.state.ByteStr

/**
  * Asset Information Retrieval Package
  */
package object air {
  type AssetName = String // Local typing for associating AssetName with a String

  type PublicKey = Array[Byte]
}
