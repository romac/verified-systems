
package actors

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object fib {

  def range(from: BigInt, to: BigInt): List[BigInt] = {
    require(to >= from)

    if (from - to == 0) Cons(from, Nil())
    else Cons(from, range(from + 1, to))
  }

  def lemma_range(from: BigInt, to: BigInt): Boolean = {
    require(to >= from)
    val res = range(from, to)
    forall { (i: BigInt) =>
      res.contains(i) ==> (i >= from && i <= to)
    }
  } holds

  def fib(nth: BigInt): BigInt = {
    require(nth > 0)
    val res = range(1, nth).foldLeft((BigInt(0), BigInt(1))) { case ((b, a), _) =>
      (b, a + b)
    }
    res._1
  }

  case object FibBehav extends Behavior {

    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Compute(nth, replyTo) =>
        replyTo ! Result(nth, fib(nth))
        FibBehav

      case _ => FibBehav
    }
  }

  case object ClientBehav extends Behavior {
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = {
      ClientBehav
    }
  }

  case object Fib extends ActorRef
  case object Client extends ActorRef

  case class Result(nth: BigInt, value: BigInt) extends Msg
  case class Compute(nth: BigInt, replyTo: ActorRef) extends Msg {
    require(nth > 0)
  }

  def validBehaviors(s: ActorSystem): Boolean = {
    s.behaviors(Fib) == FibBehav &&
    s.behaviors(Client) == ClientBehav
  }

  def invariant(s: ActorSystem): Boolean = {
    validBehaviors(s) &&
    s.inboxes(Fib -> Fib).isEmpty &&
    s.inboxes(Client -> Client).isEmpty &&
    s.inboxes(Client -> Fib).forall {
      case c: Compute => true
      case _ => false
    } &&
    s.inboxes(Fib -> Client).forall {
      case r: Result => true
      case _ => false
    }
  }

  def theorem(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
    require(invariant(s))
    val newSystem = s.step(Client, Fib).step(Fib, Client)
    invariant(newSystem)
  } holds

  def lemma_sameBehaviors(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
    require(validBehaviors(s))
    assert(validBehaviors(s.step(Client, Fib)))
    validBehaviors(s.step(from, to))
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
