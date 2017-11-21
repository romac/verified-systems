package actors

import stainless.lang._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

case class ActorSystem(
  behaviors: CMap[ActorRef, Behavior],
  inboxes: CMap[(ActorRef, ActorRef), List[Msg]],
  nextActorId: CMap[ActorRef, BigInt]
) {

  def step(from: ActorRef, to: ActorRef): ActorSystem = {
    inboxes(from -> to) match {
      case Nil() =>
        this

      case Cons(msg, msgs) =>
        val (newBehavior, toSend, toSpawn, nextId) = deliverMessage(to, from, msg)

        val newBehaviors = toSpawn.foldLeft(behaviors.updated(to, newBehavior)) { case (acc, (id, behav)) =>
          acc.updated(id, behav)
        }

        val newInboxes = toSend.foldLeft(inboxes.updated(from -> to, msgs)) {
          case (acc, Packet(dest, m)) => acc.updated(to -> dest, acc(to -> dest) :+ m)
        }

        ActorSystem(
          newBehaviors,
          newInboxes,
          nextActorId.updated(to, nextId)
       )
    }
  }

  def deliverMessage(to: ActorRef, from: ActorRef, msg: Msg): (Behavior, List[Packet], List[(ActorRef, Behavior)], BigInt) = {
    val behavior = behaviors(to)

    val nextId = nextActorId(to)
    val ctx = ActorContext(to, nextId, Nil(), Nil())
    val nextBehavior = behavior.processMsg(msg)(ctx)

    (nextBehavior, ctx.toSend, ctx.toSpawn, ctx.nextActorId)
  }

  def isStopped(id: ActorRef): Boolean = {
    behaviors(id) == Behavior.stopped
  }

  def send(to: ActorRef, msg: Msg): ActorSystem = {
    val inbox = inboxes(Main -> to) :+ msg
    ActorSystem(behaviors, inboxes.updated(Main -> to, inbox), nextActorId)
  }

}

