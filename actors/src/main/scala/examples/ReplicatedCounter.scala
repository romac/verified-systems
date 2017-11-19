
package actors

import stainless.lang._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object replicated {

  val primary = ActorRef("primary")
  val backup = ActorRef("backup")

  case class Primary(counter: Counter) extends Behavior {
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Inc() =>
        backup ! Inc()
        Primary(counter.increment)
    }
  }

  case class Backup(counter: Counter) extends Behavior {
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Inc() =>
        Backup(counter.increment)
    }
  }

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
    s.inboxes(backup -> backup).isEmpty && {
      (s.behaviors(primary), s.behaviors(backup)) match {
        case (Primary(p), Backup(b)) =>
          p.value == b.value + s.inboxes(primary -> backup).length

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
