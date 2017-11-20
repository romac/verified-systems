
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

  abstract class ActorRef {
    def !(msg: Msg)(implicit ctx: ActorContext): Unit = {
      ctx.send(this, msg)
    }
  }

  case object Main extends ActorRef

  def stepPreservesInvariant(inv: ActorSystem => Boolean): Boolean =
    forall { (s: ActorSystem, from: ActorRef, to: ActorRef) =>
      inv(s) ==> inv(s.step(from, to))
    }

  sealed abstract class Transition
  case class Send(from: ActorRef, to: ActorRef, msg: Msg)    extends Transition
  case class Receive(from: ActorRef, to: ActorRef, msg: Msg) extends Transition
  case class Become(ref: ActorRef, behavior: Behavior)       extends Transition

  object Transition {
    def send(from: ActorRef, to: ActorRef, msg: Msg): Transition =
      Send(from, to, msg)

    def receive(from: ActorRef, to: ActorRef, msg: Msg): Transition =
      Receive(from, to, msg)

    def become(ref: ActorRef, behavior: Behavior): Transition =
      Become(ref, behavior)
  }

}

