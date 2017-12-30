
package actors

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object spawn {

  case class Primary() extends ActorRef("primary")

  case class BeforeB() extends Behavior {
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Spawn() =>
        // val child = ctx.spawn(ChildB(), "child")
        // Behavior.same
        AfterB(Primary())
    }
  }

  case class AfterB(child: ActorRef) extends Behavior {
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case _ => Behavior.same
    }
  }

  case class ChildB() extends Behavior {
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case _ => Behavior.same
    }
  }

  case class Spawn() extends Msg

  val childRef = Child("child", Primary())

  def validBehaviors(s: ActorSystem): Boolean = {
    {
      s.behaviors.contains(Primary())
    } &&
    {
      s.behaviors(Primary()) match {
        case BeforeB() => !s.behaviors.contains(childRef)
        case AfterB(child) => child == childRef && s.behaviors.contains(child)
        case _ => false
      }
    } && {
      s.behaviors.contains(childRef) ==> (s.behaviors(childRef) match {
        case ChildB() => true
        case _ => false
      })
    }
  }

  def invariant(s: ActorSystem): Boolean = {
    validBehaviors(s) && {
      s.behaviors(Primary()) match {
        case BeforeB() =>
          s.isStopped(childRef)

        case AfterB(child) =>
          child == childRef &&
          s.behaviors(childRef) == ChildB()
      }
    }
  }

  def theorem(s: ActorSystem): Boolean = {
    require(invariant(s) && s.isStopped(childRef) && s.behaviors(Primary()) == BeforeB())
    val t = s.withInbox(Primary(), Primary(), List(Spawn()))
    invariant(t.step(Primary(), Primary()))
  } holds

}
