
import stainless.lang._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

package object actors {

  abstract class Msg

  case class Packet(dest: ActorRef, payload: Msg)

  case class ActorContext(
    self: ActorRef,
    var nextActorId: BigInt,
    var toSend: List[Packet],
    var toSpawn: List[(ActorRef, Behavior)]
  ) {

    def send(to: ActorRef, msg: Msg): Unit = {
      toSend = toSend :+ Packet(to, msg)
    }

    def spawn(behavior: Behavior): ActorRef = {
      val id = Generated(nextActorId, self)
      nextActorId = nextActorId + 1
      toSpawn = toSpawn :+ (id, behavior)
      id
    }
  }

  abstract class ActorRef {
    def !(msg: Msg)(implicit ctx: ActorContext): Unit = {
      ctx.send(this, msg)
    }
  }

  case class Toplevel(id: String)                    extends ActorRef
  case class Generated(id: BigInt, parent: ActorRef) extends ActorRef

  case object Main extends ActorRef

  def stepPreservesInvariant(inv: ActorSystem => Boolean): Boolean =
    forall { (s: ActorSystem, from: ActorRef, to: ActorRef) =>
      inv(s) ==> inv(s.step(from, to))
    }

}

