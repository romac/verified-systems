
package actors

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object lock {

  // The head of `agents` holds the lock, the tail are waiting for the lock
  case class ServerB(agents: List[ActorRef]) extends Behavior {

    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Server.Lock(agent) if agents.isEmpty =>
        agent ! Agent.Grant
        ServerB(List(agent))

      case Server.Lock(agent) =>
        ServerB(agents :+ agent)

      case Server.Unlock(agent) if agents.nonEmpty =>
        val newAgents = agents.tail
        if (newAgents.nonEmpty) newAgents.head ! Agent.Grant
        ServerB(newAgents)

      case _ =>
        Behavior.same
    }
  }

  case class AgentB(holdsLock: Boolean) extends Behavior {

    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Agent.Lock =>
        Server() ! Server.Lock(ctx.self)
        Behavior.same

      case Agent.Unlock if holdsLock =>
        Server() ! Server.Unlock(ctx.self)
        AgentB(false)

      case Agent.Grant =>
        AgentB(true)

      case _ =>
        Behavior.same
    }
  }

  case class Server() extends ActorRef
  object Server {
    case class Lock(agent: ActorRef) extends Msg
    case class Unlock(agent: ActorRef) extends Msg
  }

  case class Agent(id: BigInt) extends ActorRef
  object Agent {
    case object Lock extends Msg
    case object Unlock extends Msg
    case object Grant extends Msg
  }

  def noMsgstoSelf(s: ActorSystem): Boolean = forall { (ref: ActorRef) =>
    s.inboxes(ref -> ref).isEmpty
  }

  def validBehaviors(s: ActorSystem): Boolean = {
    s.behaviors(Server()).isInstanceOf[ServerB] &&
    forall { (a: Agent) => s.behaviors(a).isInstanceOf[AgentB] }
  }

  def lemma_sameBehaviors(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
    require(invariant(s))
    validBehaviors(s.step(from, to))
  } holds

  def invariant(s: ActorSystem): Boolean = {
    noMsgstoSelf(s) && validBehaviors(s) && mutex(s) &&
    forall { (a: Agent) => hasLockThenHead(s, a) }
  }

  def hasLock(s: ActorSystem, a: Agent): Boolean = {
    s.behaviors(a) match {
      case AgentB(hasLock) => hasLock
      case _ => false
    }
  }

  def hasLockThenHead(s: ActorSystem, a: Agent): Boolean = {
    hasLock(s, a) ==> {
      s.behaviors(Server()) match {
        case ServerB(Cons(head, _)) => head == a
        case _ => false
      }
    }
  }

  def mutex(s: ActorSystem): Boolean = forall { (a: Agent, b: Agent) =>
    (a != b) ==> !(hasLock(s, a) && hasLock(s, b))
  }

  def theorem(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
    require(invariant(s))
    invariant(s.step(from, to))
  } holds

}
