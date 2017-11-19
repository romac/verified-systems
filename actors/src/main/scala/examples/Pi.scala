// https://doc.akka.io/docs/akka/2.0/intro/getting-started-first-scala.html

package actors

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object pi {

  case class MasterB() extends Behavior {

    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Work(start, numOfElements) =>
        Worker ! Result(calc(start, numOfElements))
        Behavior.same

      case _ =>
        Behavior.same
    }
  }

  case class WorkerB() extends Behavior {

    def calc(start: BigInt, numOfElements: BigInt): Double = {
      var acc = 0.0
      // for (i ← start until (start + nrOfElements))
      //   acc += 4.0 * (1 - (i % 2) * 2) / (2 * i + 1)
      acc
    }

    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Work(start, numOfElement) =>
        Worker ! Result(calc(start, numOfElements))
        Behavior.same

      case _ =>
        Behavior.same
    }
  }

  case object Master extends ActorRef
  case object Worker extends ActorRef

  case object Calculate extends Msg
  case class Work(start: BigInt, numOfElements: BigInt) extends Msg
  case class Result(value: Double) extends Msg
  case class PiApprox(pi: Double) extends Msg

  def validBehaviors(s: ActorSystem): Boolean = {
    s.behaviors(Master).isInstanceOf[MasterB] &&
    s.behaviors(Worker).isInstanceOf[WorkerB]
  }

  def lemma_sameBehaviors(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
    require(invariant(s))
    assert(validBehaviors(s.step(Master, Worker)))
    assert(validBehaviors(s.step(Worker, Master)))
    validBehaviors(s.step(from, to))
  } holds

  def invariant(s: ActorSystem): Boolean = {
    validBehaviors(s) &&
    s.inboxes(Master -> Master).isEmpty &&
    s.inboxes(Worker -> Worker).isEmpty &&
    s.inboxes(Master -> Worker).forall {
      case w: Work => true
      case _ => false
    } &&
    s.inboxes(Worker -> Master).forall {
      case r: Result => true
      case _ => false
    }
  }

  def theorem(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
    require(invariant(s))
    val newSystem = s.step(from, to)
    invariant(newSystem)
  } holds

//   def lemma(s: ActorSystem): Boolean = {
//     require(invariant(s))

//     s.inboxes(Primary() -> Backup()) match {
//       case Nil() => s.step(Primary(), Backup()) == s
//       case Cons(Deliver(c), rest) =>
//         assert(lemma_sameBehaviors(s, Primary(), Backup()))
//         val BackBehav(b) = s.step(Primary(), Backup()).behaviors(Backup())
//         b.value == c.value
//     }
//   } holds

}
