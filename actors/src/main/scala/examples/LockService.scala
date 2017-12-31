
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

  sealed abstract class AgentId
  case object A extends AgentId
  case object B extends AgentId
  case object C extends AgentId

  case class Agent(id: AgentId) extends ActorRef
  object Agent {
    case object Lock   extends Msg
    case object Unlock extends Msg
    case object Grant  extends Msg
  }

  def validBehaviors(s: ActorSystem): Boolean = {
    s.behaviors(Server()).isInstanceOf[ServerB] &&
    s.behaviors(Agent(A)).isInstanceOf[AgentB]  &&
    s.behaviors(Agent(B)).isInstanceOf[AgentB]  &&
    s.behaviors(Agent(C)).isInstanceOf[AgentB]
  }

  def sameBehaviors(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
    require(invariant(s))
    validBehaviors(s.step(from, to))
  } holds

  def hasLock(s: ActorSystem, a: ActorRef): Boolean = {
    s.behaviors(a) match {
      case AgentB(hasLock) => hasLock
      case _ => false
    }
  }

  def hasLockThenHead(s: ActorSystem, a: ActorRef): Boolean = {
    hasLock(s, a) ==> {
      s.behaviors(Server()) match {
        case ServerB(Cons(head, _)) => head == a
        case _ => false
      }
    }
  }

  def mutex(s: ActorSystem): Boolean = forall { (a: ActorRef, b: ActorRef) =>
    (a != b) ==> !(hasLock(s, a) && hasLock(s, b))
  }

  def invariant(s: ActorSystem): Boolean = {
    validBehaviors(s)            &&
    mutex(s)                     &&
    hasLockThenHead(s, Agent(A)) &&
    hasLockThenHead(s, Agent(B)) &&
    hasLockThenHead(s, Agent(C))
  }

  def emptyInboxes(s: ActorSystem): Boolean = {
    s.inboxes(Server() -> Agent(A)).isEmpty &&
    s.inboxes(Server() -> Agent(B)).isEmpty &&
    s.inboxes(Server() -> Agent(C)).isEmpty &&
    // s.inboxes(Agent(A) -> Server()).isEmpty &&
    s.inboxes(Agent(B) -> Server()).isEmpty &&
    s.inboxes(Agent(C) -> Server()).isEmpty &&
    s.inboxes(Agent(A) -> Agent(A)).isEmpty &&
    s.inboxes(Agent(A) -> Agent(B)).isEmpty &&
    s.inboxes(Agent(A) -> Agent(C)).isEmpty &&
    s.inboxes(Agent(B) -> Agent(A)).isEmpty &&
    s.inboxes(Agent(B) -> Agent(B)).isEmpty &&
    s.inboxes(Agent(B) -> Agent(C)).isEmpty &&
    s.inboxes(Agent(C) -> Agent(A)).isEmpty &&
    s.inboxes(Agent(C) -> Agent(B)).isEmpty &&
    s.inboxes(Agent(C) -> Agent(C)).isEmpty
  }

  def theorem(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
    require(invariant(s) && emptyInboxes(s))
    invariant(s.step(Agent(A), Server()))
  } holds

}
