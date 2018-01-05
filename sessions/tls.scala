
import stainless.lang._
import stainless.linear._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

import sessions._

// https://tools.ietf.org/html/rfc5246
object tls {

  case class HandshakeMessage()

  implicit val timeout: Duration = Duration(500)

  case class HandshakeFailure()

  sealed abstract class AlertLevel(val level: Int)
  case object Warning extends AlertLevel(1)
  case object Fatal   extends AlertLevel(2)

  sealed abstract class AlertDescription(val id: Int)
  case object CloseNotify extends AlertDescription(0)
  case object UnexpectedMessage extends AlertDescription(10)
  // ....

  case class Alert(level: AlertLevel, description: AlertDescription)

  sealed abstract class KeyExchange
  object KeyExchange {
    case object RSA     extends KeyExchange
    case object DH_RSA  extends KeyExchange
    case object DH_anon extends KeyExchange // no certificate
  }

  sealed abstract class BlockCipher
  object BlockCipher {
    case object DES3_EDE_CBC extends BlockCipher
  }

  sealed abstract class MAC
  object MAC {
    case object SHA extends MAC
  }

  sealed abstract class CompressionMethod
  object CompressionMethod {
    case object Null extends CompressionMethod
    case object Gzip extends CompressionMethod
  }

  sealed abstract class ProtocolVersion
  case object TLS_12 extends ProtocolVersion

  case class CipherSuite(
    keyExchange: KeyExchange,
    blockCipher: BlockCipher,
    mac: MAC
  )

  case class Random(data: Array[Byte]) {
    require(data.length == 28)
  }

  // case class SessionId(id: Long)

  case class ClientHello(
    clientVersion: ProtocolVersion,
    random: Random,
    // sessionId: Option[SessionId],
    cipherSuites: List[CipherSuite],
    compressionMethods: List[CompressionMethod]
  ) {
    require {
      cipherSuites.nonEmpty &&
      compressionMethods.contains(CompressionMethod.Null)
    }
  }

  case class ServerHello(
    serverVersion: ProtocolVersion,
    random: Random,
    // sessionId: Optional[SessionId],
    cipherSuite: CipherSuite,
    compressionMethod: CompressionMethod
  )

  case class ChangeCipherSpec(cipherSuite: CipherSuite)

  case class Cert(/* opaque */)
  case class Certificate(certificate: Cert)

  case class ServerDHParams(/* opaque */)

  // only for DH_anon
  case class ServerKeyExchange(
    algo: KeyExchange,
    params: ServerDHParams
  ) {
    require(algo == KeyExchange.DH_anon)
  }

  case class ServerHelloDone()

  sealed abstract class ExchangeKeys
  case class EncryptedPreMasterSecret(/* opaque */)  extends ExchangeKeys
  case class ClientDiffieHellmanPublic(/* opaque */) extends ExchangeKeys

  case class ClientKeyExchange(
    algo: KeyExchange,
    exchangeKeys: ExchangeKeys
  ) {
    /* todo: require valid keys depending on algo */
  }

  val VerifyDataLength = 12

  case class Finished(
    verifyData: Array[Byte],
    finishedLabel: String,
    handshakeMessages: List[HandshakeMessage]
  ) {
    require {
      verifyData.length == VerifyDataLength &&
      handshakeMessages.length >= 6
    }
  }

}

