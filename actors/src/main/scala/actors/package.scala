
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

  case class Child(name: String, by: ActorRef) extends ActorRef(name, Some(by))

  case class ActorContext(
    self: ActorRef,
    var toSend: List[Packet],
    var toSpawn: List[(ActorRef, Behavior)]
  ) {

    def send(to: ActorRef, msg: Msg): Unit = {
      toSend = toSend :+ Packet(to, msg)
    }

    def spawn(behavior: Behavior, name: String): ActorRef = {
      val id: ActorRef = Child(name, self)
      toSpawn = toSpawn :+ (id, behavior)
      id
    }
  }

  def stepPreservesInvariant(inv: ActorSystem => Boolean): Boolean =
    forall { (s: ActorSystem, from: ActorRef, to: ActorRef) =>
      inv(s) ==> inv(s.step(from, to))
    }

}

