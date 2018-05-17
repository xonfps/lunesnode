package io.lunes.transaction

import scorex.account.PublicKeyAccount

/** Trait for Authorized objects. */
trait Authorized {
  val sender: PublicKeyAccount
}
