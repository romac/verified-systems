package actors

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

import crdt.GCounter

object distr {

  val agents = List(Agent(1), Agent(2), Agent(3))

  case class AgentB(id: BigInt, counter: GCounter, dirty: Boolean) extends Behavior {

    def broadcast(value: GCounter)(implicit ctx: ActorContext): Unit = {
      agents.filter(_.id != id).map { ref =>
        ref ! Broadcast(value)
      }
    }

    override
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Inc =>
        broadcast(counter + 1)
        AgentB(counter + 1)

      case Broadcast(other) =>
        AgentB(counter merge other)
    }
  }

  case class Agent(id: BigInt) extends ActorRef

  case object Inc extends Msg
  case class Broadcast(counter: GCounter) extends Msg

  def invariant(s: ActorSystem): Boolean = {
    ???
  }

  def theorem(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
    require(invariant(s))
    invariant(s.step(from, to))
  } holds

}
