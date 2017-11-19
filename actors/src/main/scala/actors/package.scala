
import stainless.lang._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

package object actors {

  abstract class Msg

  case class Packet(dest: ActorRef, payload: Msg)

  case class ActorContext(self: ActorRef, var toSend: List[Packet]) {
    def send(to: ActorRef, msg: Msg): Unit = {
      toSend = toSend :+ Packet(to, msg)
    }
  }

  case class ActorRef(name: String) {

    require(name.bigLength > 0)

    def !(msg: Msg)(implicit ctx: ActorContext): Unit = {
      ctx.send(this, msg)
    }
  }

  def stepPreservesInvariant(inv: ActorSystem => Boolean): Boolean =
    forall { (s: ActorSystem, from: ActorRef, to: ActorRef) =>
      inv(s) ==> inv(s.step(from, to))
    }

}

