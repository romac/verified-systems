import stainless.lang._
import stainless.collection._
import stainless.math.max

package object crdt {

  case class ActorRef(id: BigInt)
  case class ActorContext(self: ActorRef, actors: List[ActorRef])

  implicit class ListOps[A](val list: List[A]) {
    // def isDistinct: Boolean = list.unique == list

    def isDistinct: Boolean = list match {
      case Nil() => true
      case Cons(x, xs) => !xs.contains(x) && xs.isDistinct
    }
  }

  object MaxTheorems {

    def max_commutative(a: BigInt, b: BigInt): Boolean = {
      max(a, b) == max(b, a)
    }

    def max_idempotent(a: BigInt): Boolean = {
      max(a, a) == a
    }

    def max_associative(a: BigInt, b: BigInt, c: BigInt): Boolean = {
      max(max(a, b), c) == max(a, max(b, c))
    }

    def max_monotonic(a: BigInt, b: BigInt): Boolean = {
      val c = max(a, b)
      a <= c && b <= c
    }

    def max_semilattice: Boolean = {
      forall((a: BigInt, b: BigInt)            => max_commutative(a, b)) &&
      forall((a: BigInt)                       => max_idempotent(a)) &&
      forall((a: BigInt, b: BigInt, c: BigInt) => max_associative(a, b, c)) &&
      forall((a: BigInt, b: BigInt)            => max_monotonic(a, b))
    } holds

  }

}
