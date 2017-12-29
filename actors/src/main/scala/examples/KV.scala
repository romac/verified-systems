package actors

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object kv {

  case class ServerB(data: Map[String, String]) extends Behavior {
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Put(key, value) =>
        ServerB(data.updated(key, value))

      case Get(key, replyTo) =>
        replyTo ! Data(key, data.get(key))
        ServerB(data)

      case _ =>
        ServerB(data)
    }
  }

  case object Server extends ActorRef

  case class Put(key: String, value: String) extends Msg
  case class Get(key: String, replyTo: ActorRef) extends Msg
  case class Data(key: String, value: Option[String]) extends Msg

  def noMsgsToSelf(s: ActorSystem): Boolean = forall { (ref: ActorRef) =>
    s.inboxes(ref -> ref).isEmpty
  }

  def validBehaviors(s: ActorSystem): Boolean = {
    s.behaviors(Main) == Behavior.stopped &&
    (s.behaviors(Server) match {
      case ServerB(_) => true
      case _ => false
    })
  }

  def invariant(s: ActorSystem): Boolean = {
    noMsgsToSelf(s) && validBehaviors(s)
  }

  def noMsgInFlight(s: ActorSystem): Boolean = forall { (from: ActorRef, to: ActorRef) =>
    s.inboxes(from -> to).isEmpty
  }

  def setThenGet(init: ActorSystem, key: String, value: String): Boolean = {
    require(invariant(init) && noMsgInFlight(init) && init.trace.isEmpty)

    val res = init
      .send(Main, Server, Put(key, value))
      .send(Main, Server, Get(key, Main))
      // .step(Main, Server)
      // .step(Main, Server)
      // .step(Server, Main)
      //

    val trace = res.trace
    check(trace.nonEmpty)
  } holds

  // def theorem(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
  //   require(invariant(s))
  //   invariant(s.step(from, to))
  // } holds

}
