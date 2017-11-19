
import stainless.lang._
import stainless.lang.utils._
import stainless.data._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

package object actors {

  abstract class Msg

  case class Packet(dest: ActorId, payload: Msg)

  case class ActorContext(self: ActorId, var toSend: List[Packet]) {
    def send(to: ActorId, msg: Msg): Unit = {
      toSend = toSend :+ Packet(to, msg)
    }
  }

  abstract class ActorId {
    def !(msg: Msg)(implicit ctx: ActorContext): Unit = {
      ctx.send(this, msg)
    }
  }

  abstract class Behavior {

    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior

    @inline
    implicit def sameBehavior: Behavior = this
  }

  object Behavior {
    case class Stopped() extends Behavior {
      def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = {
          Behavior.same
        }
    }

    @inline
    def same(implicit behavior: Behavior): Behavior =
      behavior

    @inline
    def stopped: Stopped =
      Stopped()
  }

  case class ActorSystem(
    behaviors: CMap[ActorId, Behavior],
    inboxes: CMap[(ActorId, ActorId), List[Msg]]
  ) {

    def step(from: ActorId, to: ActorId): ActorSystem = {
      inboxes(from -> to) match {
        case Nil() => this
        case Cons(msg, msgs) =>
          val (newBehavior, toSend) = deliverMessage(to, msg)

          val newBehaviors = behaviors.updated(to, newBehavior)
          val newInboxes = toSend.foldLeft(inboxes.updated(from -> to, msgs)) {
            case (acc, Packet(dest, m)) => acc.updated(to -> dest, acc(to -> dest) :+ m)
          }

          ActorSystem(newBehaviors, newInboxes)
      }
    }

    def deliverMessage(actor: ActorId, msg: Msg): (Behavior, List[Packet]) = {
      val ctx      = ActorContext(actor, Nil())
      val behavior = behaviors(actor)

      val nextBehavior = behavior.processMsg(msg)(ctx)
      (nextBehavior, ctx.toSend)
    }

    def isStopped(id: ActorId): Boolean = {
      behaviors(id) == Behavior.stopped
    }

  }

}

