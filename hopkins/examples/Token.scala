
// https://ethereum.org/token

package hopkins

import stainless.lang._
import stainless.lang.utils._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object initialBehavior {
  import replicated._

  def forActor(actor: ActorId): Behavior = actor match {
    case _ => Behavior.stopped
  }
}

object token {

  val initialSupply: BigInt = 1270000

  case class Address(toInt: BigInt)

  case class Money(toInt: BigInt) {
    require(toInt >= 0)
  }

  case class ReceiveApproval(from: Address, value: Money, token: Address) extends Msg

  case class TokenBehavior(
    balanceOf: CMap[Address, Money],

  ) extends Behavior {
  
  }

}
