
package actors

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object transformer {

  case class Counter(value: BigInt) {
    require(value >= 0)

    @inline
    def increment: Counter =
      Counter(value + 1)

    @inline
    def <=(that: Counter): Boolean = {
      this.value <= that.value
    }
  }

  case class Counting(counter: Counter) extends Behavior {
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Inc => Counting(counter.increment)
      case _ => Behavior.same
    }
  }

  case class Replicated(underlying: Behavior, queue: List[Msg], replica: ActorRef) extends Behavior {
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Replicate(_) =>
        Behavior.same

      case Ack if queue.nonEmpty =>
        val newBehavior = underlying.processMsg(queue.head)
        val newQueue = queue.tail
        if (newQueue.nonEmpty) {
          replica ! Replicate(msg)
        }
        Replicated(underlying, newQueue, replica)

      case Ack =>
        Behavior.same

      case msg =>
        val newQueue = queue :+ msg
        if (newQueue.size == 1) {
          replica ! Replicate(msg)
        }
        Replicated(underlying, newQueue, replica)
    }
  }

  case class Replica(underlying: Behavior, primary: ActorRef) extends Behavior {
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Replicate(msg) =>
        primary ! Ack
        underlying.processMsg(msg)

      case _ => Behavior.same
    }
  }

  case object Primary extends ActorRef
  case object Backup  extends ActorRef

  case object Inc extends Msg
  case class Replicate(msg: Msg) extends Msg

  case object Ack extends Msg

  def repIncMsgs(inbox: List[Msg]): List[Msg] = inbox.filter {
    case Replicate(Inc) => true
    case _ => false
  }

  def incMsgs(inbox: List[Msg]): List[Msg] = inbox.filter {
    case Inc => true
    case _ => false
  }

  def invariant(s: ActorSystem): Boolean = {
    s.inboxes(Backup -> Backup).isEmpty &&
    s.inboxes(Primary -> Primary).isEmpty && {
      (s.behaviors(Primary), s.behaviors(Backup)) match {
        case (Replicated(Counting(p), queue, Backup), Replica(Counting(b), Primary)) =>
          val pendingInc = repIncMsgs(s.inboxes(Primary -> Backup)).length + incMsgs(queue).length
          p.value == b.value + pendingInc

        case _ => false
      }
    }
  }

  def theorem(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
    require(invariant(s))
    val newSystem = s.step(from, to)
    invariant(newSystem)
  } holds

}
