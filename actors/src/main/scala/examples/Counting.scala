
package actors

import stainless.lang._
import stainless.lang.utils._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object counting {

  case class PrimBehav(counter: Counter) extends Behavior {

    override
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Inc() =>
        Backup() ! Deliver(counter.increment)
        PrimBehav(counter.increment)

      case _ => Behavior.same
    }
  }

  case class BackBehav(counter: Counter) extends Behavior {

    override
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Deliver(c) => BackBehav(c)
      case _ => Behavior.same
    }


  }

  case class Primary() extends ActorId
  case class Backup()  extends ActorId

  case class Inc() extends Msg
  case class Deliver(c: Counter) extends Msg

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

  def isSorted(list: List[Msg]): Boolean = list match {
    case Nil() => true
    case Cons(Deliver(_), Nil()) => true
    case Cons(Deliver(Counter(a)), rest@Cons(Deliver(Counter(b)), xs)) => a < b && isSorted(rest)
    case _ => false // we also reject if the list contains other messages than Deliver
  }

  def invariant(s: ActorSystem): Boolean = {
    s.inboxes((Backup(), Backup())).isEmpty && {
      (s.behaviors(Primary()), s.behaviors(Backup())) match {
        case (PrimBehav(p), BackBehav(b)) =>
          val bInbox = s.inboxes((Primary(), Backup()))
          p.value >= b.value && isSorted(bInbox) && bInbox.forall {
            case Deliver(Counter(i)) => p.value >= i
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
