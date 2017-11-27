package actors

import stainless.lang._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

case class ActorSystem(
  name: String,
  behaviors: CMap[ActorRef, Behavior] = CMap(_ => Behavior.stopped),
  inboxes: CMap[(ActorRef, ActorRef), List[Msg]] = CMap(_ => List())
) {

  def run(): Unit = {}

  def step(from: ActorRef, to: ActorRef): ActorSystem = {
    inboxes(from -> to) match {
      case Nil() =>
        this

      case Cons(msg, msgs) =>
        val (newBehavior, toSend, toSpawn) = deliverMessage(to, from, msg)

        val newBehaviors = toSpawn.foldLeft(behaviors.updated(to, newBehavior)) { case (acc, (id, behav)) =>
          acc.updated(id, behav)
        }

        val newInboxes = toSend.foldLeft(inboxes.updated(from -> to, msgs)) {
          case (acc, Packet(dest, m)) => acc.updated(to -> dest, acc(to -> dest) :+ m)
        }

        ActorSystem(
          name,
          newBehaviors,
          newInboxes
       )
    }
  }

  def deliverMessage(to: ActorRef, from: ActorRef, msg: Msg): (Behavior, List[Packet], List[(ActorRef, Behavior)]) = {
    val behavior = behaviors(to)

    val ctx = ActorContext(to, Nil(), Nil())
    val nextBehavior = behavior.processMsg(msg)(ctx)

    (nextBehavior, ctx.toSend, ctx.toSpawn)
  }

  def isStopped(id: ActorRef): Boolean = {
    behaviors(id) == Behavior.stopped
  }

  def send(from: ActorRef, to: ActorRef, msg: Msg): ActorSystem = {
    val inbox = inboxes(from -> to) :+ msg
    ActorSystem(name, behaviors, inboxes.updated(from -> to, inbox))
  }

}

