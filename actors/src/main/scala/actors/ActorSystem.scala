package actors

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import stainless.util.Random
import stainless.util.Random.State

import scala.language.postfixOps

case class ActorSystem(
  behaviors: CMap[ActorRef, Behavior],
  inboxes: CMap[(ActorRef, ActorRef), List[Msg]],
  trace: List[Transition]
) {

  @library
  def run(actors: List[ActorRef], steps: BigInt)(implicit state: State): ActorSystem = {
    require(actors.nonEmpty && steps >= 0)

    val (a, b) = (Random.nextBigInt(actors.size), Random.nextBigInt(actors.size))
    val (from, to) = (actors(a), actors(b))
    val next = step(from, to)
    if (steps == 0) next
    else next.run(actors, steps - 1)
  }

  def step(from: ActorRef, to: ActorRef): ActorSystem = {
    inboxes(from -> to) match {
      case Nil() =>
        this

      case Cons(msg, msgs) =>
        val (newBehavior, toSend, trans) = deliverMessage(to, from, msg)

        val newBehaviors = behaviors.updated(to, newBehavior)
        val newInboxes = toSend.foldLeft(inboxes.updated(from -> to, msgs)) {
          case (acc, Packet(dest, m)) => acc.updated(to -> dest, acc(to -> dest) :+ m)
        }

        ActorSystem(newBehaviors, newInboxes, trace ++ trans)
    }
  }

  def deliverMessage(to: ActorRef, from: ActorRef, msg: Msg): (Behavior, List[Packet], List[Transition]) = {
    val behavior = behaviors(to)

    val ctx = ActorContext(to, Nil())
    val nextBehavior = behavior.processMsg(msg)(ctx)

    val trans = List(
      // Transition.receive(from, to, msg),
      Transition.become(to, nextBehavior)
    ) ++ ctx.toSend.map {
      case Packet(dest, msg) => Transition.send(to, dest, msg)
    }

    (nextBehavior, ctx.toSend, trans)
  }

  def isStopped(id: ActorRef): Boolean = {
    behaviors(id) == Behavior.stopped
  }

  def send(to: ActorRef, msg: Msg): ActorSystem = {
    val inbox = inboxes(Main -> to) :+ msg
    ActorSystem(behaviors, inboxes.updated(Main -> to, inbox), trace)
  }

  def transition(trans: Transition): ActorSystem = trans match {
    case Become(ref, behavior) =>
      ActorSystem(behaviors.updated(ref, behavior), inboxes, trace :+ trans)

    case Send(from, to, msg) =>
      val inbox = inboxes(from -> to) :+ msg
      ActorSystem(behaviors, inboxes.updated(from -> to, inbox), trace :+ trans)

    case Receive(from, to, msg) =>
      ActorSystem(behaviors, inboxes, trace :+ trans)
  }

}

