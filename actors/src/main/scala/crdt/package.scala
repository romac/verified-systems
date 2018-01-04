import stainless.lang._
import stainless.collection._

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

}
