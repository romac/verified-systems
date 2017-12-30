
import stainless.lang._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

package object actors {

  abstract class Msg

  case class Packet(dest: ActorRef, payload: Msg)

  abstract class ActorRef(name: String, parent: Option[ActorRef] = None()) {
    def !(msg: Msg)(implicit ctx: ActorContext): Unit = {
      ctx.send(this, msg)
    }
  }

  case class Child(name: String, getParent: ActorRef) extends ActorRef(name, Some(getParent))

  case class ActorContext(
    self: ActorRef,
    var toSend: List[Packet],
    var toSpawn: List[(ActorRef, Behavior)]
  ) {

    def send(to: ActorRef, msg: Msg): Unit = {
      toSend = Packet(to, msg) :: toSend
    }

    def spawn(behavior: Behavior, name: String): ActorRef = {
      val id: ActorRef = Child(name, self)
      toSpawn = (id -> behavior) :: toSpawn
      id
    }
  }

  def stepPreservesInvariant(inv: ActorSystem => Boolean): Boolean =
    forall { (s: ActorSystem, from: ActorRef, to: ActorRef) =>
      inv(s) ==> inv(s.step(from, to))
    }

}

