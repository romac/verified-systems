
package actors

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object trading {

  case object Alice extends ActorRef
  case object Bob extends ActorRef

  case object Start extends Msg
  case object AskNegotiate extends Msg
  // case object Accept extends Msg
  case object Accepted extends Msg
  // case class Offer(item: BigInt) extends Msg
  // case class Retract(item: BigInt) extends Msg
  // case object Ready extends Msg
  // case object AskReady extends Msg
  // case object NotYet extends Msg

  case class Idle(me: ActorRef, them: ActorRef) extends Behavior {
    require(me != them)

    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Start =>
        them ! AskNegotiate
        IdleWait(me, them)

      case AskNegotiate =>
        them ! Accepted

      case _ => Behavior.same
    }
  }

  case class IdleWait(me: ActorRef, them: ActorRef) extends Behavior {
    require(me != them)

    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Accepted =>
        Negotiate(me, them)

      case _ => Behavior.same
    }
  }

  case class Negotiate(me: ActorRef, them: ActorRef) extends Behavior {
    require(me != them)

    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case _ => Behavior.same
    }
  }

  // case class Wait() extends Behavior {
  //   def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
  //     case _ => Behavior.same
  //   }
  // }

  // case class Ready() extends Behavior {
  //   def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
  //     case _ => Behavior.same
  //   }
  // }

}
