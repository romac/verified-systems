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

        ActorSystem(newBehaviors, newInboxes, trans :: trace)
    }
  }

  def deliverMessage(to: ActorRef, from: ActorRef, msg: Msg): (Behavior, List[Packet], Transition) = {
    val behavior = behaviors(to)

    val ctx = ActorContext(to, Nil())
    val nextBehavior = behavior.processMsg(msg)(ctx)
    val trans = Transition(from, to, msg, nextBehavior, ctx.toSend)

    (nextBehavior, ctx.toSend, trans)
  }

  def isStopped(id: ActorRef): Boolean = {
    behaviors(id) == Behavior.stopped
  }

  def send(from: ActorRef, to: ActorRef, msg: Msg): ActorSystem = {
    val inbox = inboxes(from -> to) :+ msg
    ActorSystem(behaviors, inboxes.updated(from -> to, inbox), trace)
  }

  def sendNow(from: ActorRef, to: ActorRef, msg: Msg): ActorSystem = {
    val inbox = msg :: inboxes(from -> to)
    ActorSystem(behaviors, inboxes.updated(from -> to, inbox), trace)
  }

}

