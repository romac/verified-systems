package actors

import stainless.lang._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

case class ActorSystem(
  behaviors: CMap[ActorRef, Behavior],
  inboxes: CMap[(ActorRef, ActorRef), List[Msg]]
) {

  def step(from: ActorRef, to: ActorRef): ActorSystem = {
    inboxes(from -> to) match {
      case Nil() =>
        this

      case Cons(msg, msgs) =>
        val (newBehavior, toSend) = deliverMessage(to, msg)

        val newBehaviors = behaviors.updated(to, newBehavior)
        val newInboxes = toSend.foldLeft(inboxes.updated(from -> to, msgs)) {
          case (acc, Packet(dest, m)) => acc.updated(to -> dest, m :: acc(to -> dest))
        }

        ActorSystem(newBehaviors, newInboxes)
    }
  }

  def deliverMessage(actor: ActorRef, msg: Msg): (Behavior, List[Packet]) = {
    val behavior = behaviors(actor)

    val ctx = ActorContext(actor, Nil())
    val nextBehavior = behavior.processMsg(msg)(ctx)

    (nextBehavior, ctx.toSend)
  }

  def isStopped(id: ActorRef): Boolean = {
    behaviors(id) == Behavior.stopped
  }

  def send(from: ActorRef, to: ActorRef, msg: Msg): ActorSystem = {
    ActorSystem(behaviors, inboxes.updated(from -> to, List(msg)))
  }

}

