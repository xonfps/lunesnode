package io.lunes.transaction

import io.lunes.network.{BlockCheckpoint, Checkpoint}
import io.lunes.state.ByteStr

trait CheckpointService {

  def set(checkpoint: Checkpoint): Either[ValidationError, Unit]

  def get: Option[Checkpoint]
}

object CheckpointService {

  implicit class CheckpointServiceExt(cs: CheckpointService) {
    def isBlockValid(candidateSignature: ByteStr,
                     estimatedHeight: Int): Boolean =
      !cs.get.exists {
        _.items.exists {
          case BlockCheckpoint(h, sig) =>
            h == estimatedHeight && candidateSignature != ByteStr(sig)
        }
      }
  }

}
