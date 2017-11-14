
package stainless
package actors

import stainless.lang._
import stainless.collection._

abstract class Message
case class NotUsed() extends Message

case class Network(
  var actors: Map[ActorId, Behavior] = Map.empty[ActorId, Behavior],
  var inboxes: Map[ActorId, List[Message]] = Map.empty[ActorId, List[Message]],
  var activeActors: List[ActorId] = Nil[ActorId]()
) {
  def actorExists(id: ActorId): Boolean = {
    actors.contains(id) && inboxes.contains(id)
  }

  def actorIsActive(id: ActorId): Boolean = {
    require(actorExists(id))
    activeActors.contains(id)
  }

  def spawn(id: ActorId, behavior: Behavior): Unit = {
    println(s"spawn: $id => $behavior")
    require(!actorExists(id))
    actors = actors.updated(id, behavior)
    inboxes = inboxes.updated(id, Nil())
    activeActors = id :: activeActors
  } ensuring(_ => actorExists(id) && actorIsActive(id))

  def stop(id: ActorId): Unit = {
    require(actorExists(id) && actorIsActive(id))
    println(s"stop: $id")
    activeActors = activeActors - id
  } ensuring(_ => actorExists(id) && !actorIsActive(id))

  def send(from: ActorId, to: ActorId, msg: Message): Unit = {
    println(s"send: $from => $to: $msg")
    require(actorExists(from) && actorIsActive(from) && actorExists(to))
    val inbox = inboxes.getOrElse(to, List())
    inboxes = inboxes.updated(to, inbox :+ msg)
  }

  def getActor(id: ActorId): Behavior = {
    require(actorExists(id))
    actors(id)
  }

  def hasInFlightMessages: Boolean = {
    activeActors.exists(hasMessages)
  }

  def nextActiveActor: Option[ActorId] = {
    activeActors.headOption match {
      case Some(id) =>
        activeActors = activeActors.tail :+ id
        Some(id)

      case None() =>
        None()
    }
  }

  def takeInboxHead(id: ActorId): Option[Message] = {
    require(inboxes contains id)
    val head = inboxes(id).headOption
    if (head.isDefined) {
      inboxes = inboxes.updated(id, inboxes(id).tail)
    }
    println(s"takeInboxHead: $id => $head")
    head
  }

  def hasMessages(id: ActorId): Boolean = {
    require(inboxes contains id)
    inboxes(id).headOption.isDefined
  }

  def actorBecome(id: ActorId, behavior: Behavior): Unit = {
    println(s"actorBecome: $id => $behavior")
    require(actorIsActive(id))
    actors = actors.updated(id, behavior)
  }
}

