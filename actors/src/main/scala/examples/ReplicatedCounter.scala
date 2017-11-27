
package actors

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object replicated {

  case class Primary() extends ActorRef("primary")
  case class Backup() extends ActorRef("backup")

  case class PrimBehav(counter: Counter) extends Behavior {

    override
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Inc =>
        Backup() ! Inc
        PrimBehav(counter.increment)
    }
  }

  case class BackBehav(counter: Counter) extends Behavior {

    override
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Inc =>
        BackBehav(counter.increment)
    }
  }

  case object Inc extends Msg

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

  @inline
  def validRef(ref: ActorRef): Boolean = {
    ref == Primary() || ref == Backup()
  }

  @inline
  def noMsgToSelf(s: ActorSystem, ref: ActorRef): Boolean = {
    s.inboxes(ref -> ref).isEmpty
  }

  def invariant(s: ActorSystem): Boolean = {
    noMsgToSelf(s, Backup()) &&
    forall((ref: ActorRef) => validRef(ref)) && {
      (s.behaviors(Primary()), s.behaviors(Backup())) match {
        case (PrimBehav(p), BackBehav(b)) =>
          p.value == b.value + s.inboxes(Primary() -> Backup()).length

        case _ => false
      }
    }
  }

  def theorem(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
    require(invariant(s) && validRef(from) && validRef(to))
    val newSystem = s.step(from, to)
    invariant(newSystem)
  } holds

}
