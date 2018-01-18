
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

      case Server.Unlock(agent) if agents.nonEmpty && agents.head == agent =>
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

  case class Agent(id: AgentId) extends ActorRef
  object Agent {
    case object Lock   extends Msg
    case object Unlock extends Msg
    case object Grant  extends Msg
  }

  def validBehaviors(s: ActorSystem): Boolean = {
    s.behaviors(Server()).isInstanceOf[ServerB] &&
    s.behaviors(Agent(A)).isInstanceOf[AgentB]  &&
    s.behaviors(Agent(B)).isInstanceOf[AgentB]
  }

  def behaviorsValids(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
    require(invariant(s))
    validBehaviors(s.step(from, to))
  } holds

  def msgValids(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
    require(invariant(s))
    validMessages(s.step(from, to))
  } holds

  // def mutexValids(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
  //   require(invariant(s))
  //   mutex(s.step(from, to), from, to)
  // } holds

  // def hasLockThenHeadValids(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
  //   require(invariant(s))
  //   hasLockThenHead(s.step(from, to), from) && 
  //   hasLockThenHead(s.step(from, to), to)
  // } holds

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

  def mutex(s: ActorSystem, a: ActorRef, b: ActorRef) = {
    (a != b) ==> !(hasLock(s, a) && hasLock(s, b))
  }

  def invariant(s: ActorSystem): Boolean = {
    validBehaviors(s)            &&
    validMessages(s)             &&
    mutex(s, Agent(A), Agent(B)) &&
    hasLockThenHead(s, Agent(A)) &&
    hasLockThenHead(s, Agent(B))
  }

  def validMessages(s: ActorSystem): Boolean = {
    noMsgToSelf(s, Server())      &&
    noMsgToSelf(s, Agent(A))      &&
    noMsgToSelf(s, Agent(B))      &&
    validAgentInbox(s, Agent(A))  &&
    validAgentInbox(s, Agent(B))  &&
    validServerInbox(s, Agent(A)) &&
    validServerInbox(s, Agent(B))
  }

  def noMsgToSelf(s: ActorSystem, ref: ActorRef): Boolean = {
    s.inboxes(ref -> ref).isEmpty
  }

  def validAgentInbox(s: ActorSystem, agent: ActorRef) = {
    s.inboxes(Server() -> agent) forall {
      case Agent.Grant => true
      case _           => false
    }
  }

  def validServerInbox(s: ActorSystem, from: ActorRef): Boolean = {
    s.inboxes(from -> Server()) forall {
      case Server.Lock(id)   => id == from
      case Server.Unlock(id) => id == from
      case _                 => false
    }
  }

  def agentInv(s: ActorSystem, agent: ActorRef): Boolean = {
    require(invariant(s))
    invariant(s.step(agent, Server())) &&
    invariant(s.step(Server(), agent))
  }

  def agentAInvLock(s: ActorSystem): Boolean = {
    require {
      invariant(s) &&
      s.inboxes(Agent(A) -> Server()).nonEmpty &&
      s.inboxes(Agent(A) -> Server()).head == Server.Lock(Agent(A))
    }

    invariant(s.step(Agent(A), Server()))
  } holds

  @symeval
  def agentAInvUnlock(s: ActorSystem): Boolean = {
    require {
      invariant(s) &&
      s.behaviors(Agent(A)).asInstanceOf[AgentB].holdsLock &&
      s.behaviors(Server()).asInstanceOf[ServerB].agents.nonEmpty &&
      s.behaviors(Server()).asInstanceOf[ServerB].agents.head == Agent(A) &&
      s.inboxes(Agent(A) -> Server()).nonEmpty &&
      s.inboxes(Agent(A) -> Server()).head == Server.Unlock(Agent(A))
    }

    invariant(s.step(Agent(A), Server()))
  } holds

  def agentAInvGrant(s: ActorSystem): Boolean = {
    require {
      invariant(s) &&
      s.inboxes(Server() -> Agent(A)).nonEmpty &&
      s.inboxes(Server() -> Agent(A)).head == Agent.Grant
    }

    invariant(s.step(Server(), Agent(A)))
  } holds

  // @symeval
  // def theorem(s: ActorSystem): Boolean = {
  //   require(invariant(s))
  //   agentInv(s, Agent(A))
  // } holds

}
