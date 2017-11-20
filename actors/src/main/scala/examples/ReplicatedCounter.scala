
package actors

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._
import stainless.util.Random

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
    s.behaviors(Main) == Behavior.stopped &&
    s.inboxes(Main -> Main).isEmpty &&
    s.inboxes(Main -> Backup()).isEmpty &&
    s.inboxes(Backup() -> Backup()).isEmpty && {
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

  @extern
  def main(args: Array[String]): Unit = {
    val actors = List[ActorRef](Main, Primary(), Backup())
    val behaviors = CMap[ActorRef, Behavior] {
      case Main      => Behavior.stopped
      case Primary() => PrimBehav(Counter(0))
      case Backup()  => BackBehav(Counter(0))
    }

    val inboxes = CMap[(ActorRef, ActorRef), List[Msg]](_ => Nil())

    val init = ActorSystem(behaviors, inboxes, Nil())
    implicit val state = Random.newState

    val res = init
      .send(Primary(), Inc())
      .send(Primary(), Inc())
      .send(Primary(), Inc())
      .run(actors, 100)

    val (holds, _) = invHolds(init, res.trace) { s =>
      s.behaviors(Main) == Behavior.stopped &&
      s.inboxes(Main -> Main).isEmpty &&
      s.inboxes(Main -> Backup()).isEmpty &&
      s.inboxes(Backup() -> Backup()).isEmpty && {
        (s.behaviors(Primary()), s.behaviors(Backup())) match {
          case (PrimBehav(p), BackBehav(b)) =>
            p.value >= b.value

          case _ => false
        }
      }
    }

    println(holds)
  }

  def invHolds(init: ActorSystem, trace: List[Transition])(inv: ActorSystem => Boolean): (Boolean, ActorSystem) = {
    require(inv(init))

    trace.foldLeft((inv(init), init)) { case ((holds, sys), trans) =>
      val next = sys.transition(trans)
      (holds && inv(next), next)
    }
  }

}
