
package actors

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object replicated {

  case class Primary() extends ActorRef("primary", None())
  case class Backup() extends ActorRef("backup", None())

  case class PrimBehav(counter: Counter) extends Behavior {

    override
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Inc =>
        Backup() ! Inc
        PrimBehav(counter)
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
  def noMsgToSelf(s: ActorSystem, ref: ActorRef): Boolean = {
    s.inboxes(ref -> ref).isEmpty
  }

  def invariant(s: ActorSystem): Boolean = {
    noMsgToSelf(s, Backup()) && {
      (s.behaviors(Primary()), s.behaviors(Backup())) match {
        case (PrimBehav(p), BackBehav(b)) =>
          p.value == b.value + s.inboxes(Primary() -> Backup()).length

        case _ => false
      }
    }
  }

  def theorem(s: ActorSystem): Boolean = {
    require(invariant(s))
    val newSystem = s.withInbox(Primary(), Primary(), Inc()).step(Primary(), Primary())
    invariant(newSystem)
  } holds

}
