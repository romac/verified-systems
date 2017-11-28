
package actors

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object quicksort {

  case class SorterB(unsorted: List[BigInt], sub: Sub, parent: ActorRef) extends Behavior {

    override
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Sort() if unsorted.isEmpty =>
        parent ! Result(Nil(), sub)
        Behavior.stopped

      case Sort() =>
        val pivot = unsorted.head
        val (left, right) = unsorted.partition(_ < pivot)

        val leftSorter = ctx.spawn(SorterB(left, Left(), ctx.self), "left")
        leftSorter ! Sort()
        val rightSorter = ctx.spawn(SorterB(right, Right(), ctx.self), "right")
        rightSorter ! Sort()

        MergerB(Nil(), sub, parent)

      case _ =>
        Behavior.same
    }
  }

  case class MergerB(current: List[BigInt], sub: Sub, parent: ActorRef) extends Behavior {

    override
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Result(half, _) if current.isEmpty =>
        MergerB(half, sub, parent)

      case Result(half, from) =>
        val sorted = if (from == Left()) half ++ current else current ++ half
        parent ! Result(sorted, sub)
        Behavior.stopped

      case _ =>
        Behavior.same
    }
  }

  case class Main() extends ActorRef("main")

  sealed abstract class Sub
  case class Left() extends Sub
  case class Right() extends Sub

  // case class Sorter(parent: ActorRef, sub: Sub) extends ActorRef

  case class Sort() extends Msg
  case class Result(sorted: List[BigInt], from: Sub) extends Msg

  // def validBehaviors(s: ActorSystem): Boolean = {
  //   s.behaviors(Primary()).isInstanceOf[PrimBehav] &&
  //   s.behaviors(Backup()).isInstanceOf[BackBehav]
  // }

  // def invariant(s: ActorSystem): Boolean = {
  //   validBehaviors(s) &&
  //   s.inboxes(Primary() -> Primary()).isEmpty &&
  //   s.inboxes(Backup() -> Backup()).isEmpty &&
  //   s.inboxes(Backup() -> Primary()).isEmpty && {

  //     val PrimBehav(p) = s.behaviors(Primary())
  //     val BackBehav(b) = s.behaviors(Backup())
  //     val bInbox = s.inboxes(Primary() -> Backup())

  //     p.value >= b.value && isSorted(bInbox) && bInbox.forall {
  //       case Deliver(Counter(i)) => p.value >= i
  //       case _ => true
  //     }
  //   }
  // }

  // def theorem(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
  //   require(invariant(s))
  //   val newSystem = s.step(from, to)
  //   invariant(newSystem)
  // } holds

}
