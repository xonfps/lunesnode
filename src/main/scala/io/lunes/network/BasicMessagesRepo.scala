package io.lunes.network

import java.net.{InetAddress, InetSocketAddress}
import java.util

import com.google.common.primitives.{Bytes, Ints}
import io.lunes.mining.Miner.MaxTransactionsPerMicroblock
import io.lunes.state2.ByteStr
import scorex.account.PublicKeyAccount
import scorex.block.{Block, MicroBlock}
import scorex.network.message.Message._
import scorex.network.message._
import io.lunes.transaction.TransactionParser._
import io.lunes.transaction.{History, Transaction, TransactionParser}

import scala.util.Try

/** Object to prospect Peer Specifications. */
object GetPeersSpec extends MessageSpec[GetPeers.type] {
  override val messageCode: Message.MessageCode = 1: Byte

  override val maxLength: Int = 0

  /** Gets a deserialized [[GetPeers]] from Raw data.
    * @param bytes The input data.
    * @return Returns the Type.
    */
  override def deserializeData(bytes: Array[Byte]): Try[GetPeers.type] =
    Try {
      require(bytes.isEmpty, "Non-empty data for GetPeers")
      GetPeers
    }

  /** Gets a serialized Strem from [[GetPeers]] information.
    * @param data The GetPeers data.
    * @return The Raw Serialized Data.
    */
  override def serializeData(data: GetPeers.type): Array[Byte] = Array()
}

/** Object for Peers Specifications. */
object PeersSpec extends MessageSpec[KnownPeers] {
  private val AddressLength = 4
  private val PortLength = 4
  private val DataLength = 4

  override val messageCode: Message.MessageCode = 2: Byte

  override val maxLength: Int = DataLength + 1000 * (AddressLength + PortLength)

  /** Deserializes Data generating a possible KownPeers.
    * @param bytes The serialized Data.
    * @return Returns a Try of [[KnownPeers]].
    */
  override def deserializeData(bytes: Array[Byte]): Try[KnownPeers] = Try {
    val lengthBytes = util.Arrays.copyOfRange(bytes, 0, DataLength)
    val length = Ints.fromByteArray(lengthBytes)

    assert(bytes.length == DataLength + (length * (AddressLength + PortLength)), "Data does not match length")

    KnownPeers((0 until length).map { i =>
      val position = lengthBytes.length + (i * (AddressLength + PortLength))
      val addressBytes = util.Arrays.copyOfRange(bytes, position, position + AddressLength)
      val address = InetAddress.getByAddress(addressBytes)
      val portBytes = util.Arrays.copyOfRange(bytes, position + AddressLength, position + AddressLength + PortLength)
      new InetSocketAddress(address, Ints.fromByteArray(portBytes))
    })
  }

  /** Serializes data from KnowPeers.
    * @param peers The input KnownPeers.
    * @return The RAW serialized data.
    */
  override def serializeData(peers: KnownPeers): Array[Byte] = {
    val length = peers.peers.size
    val lengthBytes = Ints.toByteArray(length)

    val xs = for {
      inetAddress <- peers.peers
      address <- Option(inetAddress.getAddress)
    } yield (address.getAddress, inetAddress.getPort)

    xs.foldLeft(lengthBytes) { case (bs, (peerAddress, peerPort)) =>
      Bytes.concat(bs, peerAddress, Ints.toByteArray(peerPort))
    }
  }
}

/** Signature Sequence of Specifications trait.
  * @tparam A Parametrized Type descendent of AnyRef.
  */
trait SignaturesSeqSpec[A <: AnyRef] extends MessageSpec[A] {

  import io.lunes.transaction.TransactionParser.SignatureLength

  private val DataLength = 4

  /** Defines an interface for Wrapping the Data.
    * @param signatures Inputs a Sequence of Signatures as Array[Byte].
    * @return Returns an object of the Parametrized Types.
    */
  def wrap(signatures: Seq[Array[Byte]]): A

  /** Defines an interface for Unwrapping the Data.
    * @param v Inputs the wrapped object of A.
    * @return Returns the RAW data.
    */
  def unwrap(v: A): Seq[Array[Byte]]

  override val maxLength: Int = DataLength + (200 * SignatureLength)

  /** Deserializes the input Data.
    * @param bytes The Raw serialized data.
    * @return Returns a Try for the parametrized type.
    */
  override def deserializeData(bytes: Array[Byte]): Try[A] = Try {
    val lengthBytes = bytes.take(DataLength)
    val length = Ints.fromByteArray(lengthBytes)

    assert(bytes.length == DataLength + (length * SignatureLength), "Data does not match length")

    wrap((0 until length).map { i =>
      val position = DataLength + (i * SignatureLength)
      bytes.slice(position, position + SignatureLength)
    })
  }

  /** Serializes an object of the parametrized type A.
    * @param v The input object to serialize.
    * @return Returns the serialized Raw data.
    */
  override def serializeData(v: A): Array[Byte] = {
    val signatures = unwrap(v)
    val length = signatures.size
    val lengthBytes = Ints.toByteArray(length)

    //WRITE SIGNATURES
    signatures.foldLeft(lengthBytes) { case (bs, header) => Bytes.concat(bs, header) }
  }
}

/** Object to get a Signature Specifications. */
object GetSignaturesSpec extends SignaturesSeqSpec[GetSignatures] {
  /** Wrappes a Sequence of signatures inputed as Raw data.
    * @param signatures The Sequence of Raw Signatures.
    * @return
    */
  override def wrap(signatures: Seq[Array[Byte]]): GetSignatures = GetSignatures(signatures.map(ByteStr(_)))

  /** Unwrappes Signatures to Raw Data.
    * @param v Inputs a [[GetSignatures]] object to unwrap.
    * @return
    */
  override def unwrap(v: GetSignatures): Seq[Array[MessageCode]] = v.signatures.map(_.arr)

  override val messageCode: MessageCode = 20: Byte
}

/** Signature Specifications object. */
object SignaturesSpec extends SignaturesSeqSpec[Signatures] {
  /** Wrappes a Sequence of signatures inputed as Raw data.
    * @param signatures The Sequence of Raw Signatures.
    * @return Returns the [[Signatures]] object for the wrapped data.
    */
  override def wrap(signatures: Seq[Array[Byte]]): Signatures = Signatures(signatures.map(ByteStr(_)))

  /** Unwrappes a inputed [[Signatures]] object as Raw data.
    * @param v The signatures object.
    * @return Returns a Sequence the Raw unwrapped data
    */
  override def unwrap(v: Signatures): Seq[Array[MessageCode]] = v.signatures.map(_.arr)

  override val messageCode: MessageCode = 21: Byte // Possible unnecessary Type Cast.
}

/** Get Block Specifications object.*/
object GetBlockSpec extends MessageSpec[GetBlock] {
  override val messageCode: MessageCode = 22: Byte

  override val maxLength: Int = TransactionParser.SignatureLength

  /** Serializes the data given a inputed [[GetBlock]] signature object.
    * @param signature The GetBlock input.
    * @return Returns the serialized Raw data.
    */
  override def serializeData(signature: GetBlock): Array[Byte] = signature.signature.arr

  /** Deserializes Raw data to [[GetBlock]] object.
    * @param bytes The input Raw data.
    * @return Returns a Try for a GetBlock object.
    */
  override def deserializeData(bytes: Array[Byte]): Try[GetBlock] = Try {
    require(bytes.length == maxLength, "Data does not match length")
    GetBlock(ByteStr(bytes))
  }
}

/** The Blcok Specification object. */
object BlockSpec extends MessageSpec[Block] {
  override val messageCode: MessageCode = 23: Byte

  override val maxLength: Int = 271 + TransactionSpec.maxLength * Block.MaxTransactionsPerBlockVer3

  /** Serializes data for a given [[Block]] object.
    * @param block The input Block.
    * @return Returns the Raw serialized data.
    */
  override def serializeData(block: Block): Array[Byte] = block.bytes()

  /** Deserializes Raw data into a [[Block]] object.
    * @param bytes The input Raw data.
    * @return Returns a Try of Block object.
    */
  override def deserializeData(bytes: Array[Byte]): Try[Block] = Block.parseBytes(bytes)
}

/** The ScoreSpec object. */
object ScoreSpec extends MessageSpec[History.BlockchainScore] {
  override val messageCode: MessageCode = 24: Byte

  override val maxLength: Int = 64 // allows representing scores as high as 6.6E153
  /** Serializes data given a [[History.BlockchainScore]] object.
    * @param score The BlockchainScore input.
    * @return Returns the serialized Raw data.
    */
  override def serializeData(score: History.BlockchainScore): Array[Byte] = {
    val scoreBytes = score.toByteArray
    val bb = java.nio.ByteBuffer.allocate(scoreBytes.length)
    bb.put(scoreBytes)
    bb.array()
  }

  /** Deserializes Raw data into a [[History.BlockchainScore]] object.
    * @param bytes The input Raw data.
    * @return Returns a Try for the BlockchainScore object.
    */
  override def deserializeData(bytes: Array[Byte]): Try[History.BlockchainScore] = Try {
    BigInt(1, bytes)
  }
}

/** Checkpoint Specifications object.*/
object CheckpointSpec extends MessageSpec[Checkpoint] {
  override val messageCode: MessageCode = 100: Byte

  private val HeightLength = Ints.BYTES

  override val maxLength: Int = 4 + Checkpoint.MaxCheckpoints * (HeightLength + SignatureLength)

  /** Serializes data given a [[Checkpoint]] object.
    * @param checkpoint The Checkpoint input.
    * @return Returns the serialized Raw data.
    */
  override def serializeData(checkpoint: Checkpoint): Array[Byte] =
    Bytes.concat(checkpoint.toSign, checkpoint.signature)

  /** Deserializes Raw data into a [[Checkpoint]] object.
    * @param bytes The input Raw data.
    * @return Returns a Try for the Chekckpoint object.
    */
  override def deserializeData(bytes: Array[Byte]): Try[Checkpoint] = Try {
    val lengthBytes = util.Arrays.copyOfRange(bytes, 0, Ints.BYTES)
    val length = Ints.fromByteArray(lengthBytes)

    require(length <= Checkpoint.MaxCheckpoints)

    val items = (0 until length).map { i =>
      val position = lengthBytes.length + (i * (HeightLength + SignatureLength))
      val heightBytes = util.Arrays.copyOfRange(bytes, position, position + HeightLength)
      val height = Ints.fromByteArray(heightBytes)
      val blockSignature = util.Arrays.copyOfRange(bytes, position + HeightLength, position + HeightLength + SignatureLength)
      BlockCheckpoint(height, blockSignature)
    }

    val signature = bytes.takeRight(SignatureLength)

    Checkpoint(items, signature)
  }
}

/** Transaction Specifications object. */
object TransactionSpec extends MessageSpec[Transaction] {
  override val messageCode: MessageCode = 25: Byte

  // Modeled after MassTransferTransaction https://lunes.atlassian.net/wiki/spaces/MAIN/pages/386171054/Mass+Transfer+Transaction
  override val maxLength: Int = 5000

  /** Deserializes Raw data into a Transaction object.
    * @param bytes The input Raw data.
    * @return Returns a Try for the Transaction object.
    */
  override def deserializeData(bytes: Array[Byte]): Try[Transaction] =
    TransactionParser.parseBytes(bytes)

  /** Serializes a [[Transaction]] object.
    * @param tx The input Transaction.
    * @return Returns the serialized Raw data.
    */
  override def serializeData(tx: Transaction): Array[Byte] = tx.bytes()
}

/** MicroBlockInv Specifications object. */
object MicroBlockInvSpec extends MessageSpec[MicroBlockInv] {
  override val messageCode: MessageCode = 26: Byte

  /** Deserializes Raw data to [[MicroBlockInv]] object.
    * @param bytes The input Raw data.
    * @return Returns a Try for a MicroBlockInv.
    */
  override def deserializeData(bytes: Array[Byte]): Try[MicroBlockInv] =
    Try(MicroBlockInv(
      sender = PublicKeyAccount.apply(bytes.take(KeyLength)),
      totalBlockSig = ByteStr(bytes.view.slice(KeyLength, KeyLength + SignatureLength).toArray),
      prevBlockSig = ByteStr(bytes.view.slice(KeyLength + SignatureLength, KeyLength + SignatureLength * 2).toArray),
      signature = ByteStr(bytes.view.slice(KeyLength + SignatureLength * 2, KeyLength + SignatureLength * 3).toArray)))

  /** Serializes data given a [[MicroBlockInv]] object.
    * @param inv The [[MicroBlockInv]] input.
    * @return Returns the serialized Raw data.
    */
  override def serializeData(inv: MicroBlockInv): Array[Byte] = {
    inv.sender.publicKey ++ inv.totalBlockSig.arr ++ inv.prevBlockSig.arr ++ inv.signature.arr
  }

  override val maxLength: Int = 300
}

object MicroBlockRequestSpec extends MessageSpec[MicroBlockRequest] {
  override val messageCode: MessageCode = 27: Byte

  /** Deserializes Raw data into a [[MicroBlockRequest]] object.
    * @param bytes The input data.
    * @return Returns a Try for a MicroBlockRequest object.
    */
  override def deserializeData(bytes: Array[Byte]): Try[MicroBlockRequest] =
    Try(MicroBlockRequest(ByteStr(bytes)))

  /** Serializes data given a [[MicroBlockRequest]] object.
    * @param req The MicroBlockRequest input.
    * @return Returns the serialized Raw data.
    */
  override def serializeData(req: MicroBlockRequest): Array[Byte] = req.totalBlockSig.arr

  override val maxLength: Int = 500
}

object MicroBlockResponseSpec extends MessageSpec[MicroBlockResponse] {
  override val messageCode: MessageCode = 28: Byte

  /** Deserializes raw data into a [[MicroBlockResponse]] object.
    * @param bytes The input Raw data object.
    * @return Returns a Try for the MicroBlockResponse object.
    */
  override def deserializeData(bytes: Array[Byte]): Try[MicroBlockResponse] =
    MicroBlock.parseBytes(bytes).map(MicroBlockResponse)

  /** Serializes data given a [[MicroBlockResponse]] object.
    * @param resp The MicroBlockResponse input.
    * @return The Serialized Raw data.
    */
  override def serializeData(resp: MicroBlockResponse): Array[Byte] = resp.microblock.bytes()

  override val maxLength: Int = 271 + TransactionSpec.maxLength * MaxTransactionsPerMicroblock

}

/**
  *
  */
// Virtual, only for logs
object HandshakeSpec {
  val messageCode: MessageCode = 101: Byte
}

/** Basic Message Repository object.*/
object BasicMessagesRepo {
  /** Specified Type for Message Specifications. */
  type Spec = MessageSpec[_ <: AnyRef]

  val specs: Seq[Spec] = Seq(GetPeersSpec, PeersSpec, GetSignaturesSpec, SignaturesSpec,
    GetBlockSpec, BlockSpec, ScoreSpec, CheckpointSpec, TransactionSpec,
    MicroBlockInvSpec, MicroBlockRequestSpec, MicroBlockResponseSpec)

  val specsByCodes: Map[Byte, Spec] = specs.map(s => s.messageCode -> s).toMap
  val specsByClasses: Map[Class[_], Spec] = specs.map(s => s.contentClass -> s).toMap
}