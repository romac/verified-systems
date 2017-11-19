
package actors

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object replicated {

  case class PrimBehav(counter: Counter) extends Behavior {

    override
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Inc() =>
        Backup() ! Inc()
        PrimBehav(counter.increment)
    }
  }

  case class BackBehav(counter: Counter) extends Behavior {

    override
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Inc() =>
        BackBehav(counter.increment)
    }


  }

  case class Primary() extends ActorRef
  case class Backup()  extends ActorRef

  case class Inc() extends Msg

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

  def invariant(s: ActorSystem): Boolean = {
    s.inboxes((Backup(), Backup())).isEmpty && {
      (s.behaviors(Primary()), s.behaviors(Backup())) match {
        case (PrimBehav(p), BackBehav(b)) =>
          p.value == b.value + s.inboxes(Primary() -> Backup()).length

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
