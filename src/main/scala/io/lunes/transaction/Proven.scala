package io.lunes.transaction

/** Trait to define an interface for Proven and Authorized object.*/
trait Proven extends Authorized {
  /** Returns the Proofs for  the Authorized object.
     * @return The Proofs object.
    */
  def proofs: Proofs
}
