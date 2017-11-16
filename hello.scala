
import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

case class CMap[A, B](f: A => B) {

  def apply(k: A): B = {
    f(k)
  }

  def updated(k: A, v: B): CMap[A, B] = {
    CMap((x: A) => if (x == k) v else f(x))
  }

  def getOrElse(k: A, v: B): B = {
    f(k)
  }

  def contains(k: A): Boolean =
    true

}

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


object Test {
  case class PrimBehav(counter: BigInt) extends Behavior {

    override
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Inc() =>
        Backup() ! Deliver(counter + 1)
        PrimBehav(counter + 1)

      case _ => Behavior.same
    }
  }

  case class BackBehav(counter: BigInt) extends Behavior {
    override
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Deliver(c) => BackBehav(c)
      case _ => Behavior.same
    }
  }

  case class Primary() extends ActorId
  case class Backup()  extends ActorId

  case class Inc() extends Msg
  case class Deliver(c: BigInt) extends Msg

  def invariant(s: ActorSystem): Boolean = {
    s.inboxes((Backup(), Backup())).isEmpty && {
      (s.behaviors(Primary()), s.behaviors(Backup())) match {
        case (PrimBehav(p), BackBehav(b)) =>
          val bInbox = s.inboxes((Primary(), Backup()))
          p >= b && bInbox.forall {
            case Deliver(i) => p >= i
            case _ => true
          }

          case _ => false
      }
    }
  }

  def theorem(s: ActorSystem, from: ActorId, to: ActorId): Boolean = {
    require(invariant(s))
    val newSystem = s.step(from, to)
    invariant(newSystem)
  } holds
}
