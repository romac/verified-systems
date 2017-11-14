package stainless
package actors

import stainless.lang._
import stainless.collection._

import scala.annotation.unchecked

abstract class ActorId

case class ActorRef(id: ActorId) {
  def !(msg: Message)(implicit ctx: ActorContext, net: Network): Unit = {
    ctx.send(id, msg)
  }
}

case class ActorSystem(guardianBehavior: Behavior, id: ActorId) {
  val ref = ActorRef(id)

  private def loop()(implicit network: Network): Unit = {
    if (!network.hasInFlightMessages) {
      return
    }

    val id = network.nextActiveActor.get

    val actor = network.getActor(id)
    val msg = network.takeInboxHead(id)

    println(s"loop: $actor <- $msg")

    (msg, actor) match {
      case (None(), _) =>
        loop()

      case (Some(msg), behavior: ExtensibleBehavior) =>
        val ctx = ActorContext(ActorRef(id))

        behavior.receiveMessage(ctx, msg) match {
          case Behavior.Same() =>
            network.actorBecome(id, behavior)

          case b @ Behavior.Stopped() =>
            network.actorBecome(id, b)
            network.stop(id)

          case b @ Behavior.Unhandled() =>
            network.actorBecome(id, b)
            network.stop(id)

          case other =>
            network.actorBecome(id, other)
        }

        loop()

      case (Some(msg), factory: Actor.Deferred) =>
        val ctx = ActorContext(ActorRef(id))

        // What if factory returns eg. Same?
        network.actorBecome(id, factory(ctx))
        loop()

      case (_, Behavior.Same()) => // never happens
        loop()

      case (_, Behavior.Stopped()) =>
        network.stop(id)
        loop()

      case (_, Behavior.Unhandled()) =>
        loop()
    }
  }

  def run(implicit network: Network): Unit = {
    implicit val ctx = ActorContext(ref)
    val self = ctx.spawn(guardianBehavior, id)
    self ! NotUsed()
    loop()
    println("======= DONE ======")
    println(network)
  }
}

case class ActorContext(self: ActorRef) {
  def spawn(behavior: Behavior, id: ActorId)(implicit network: Network): ActorRef = {
    network.spawn(id, behavior)
    ActorRef(id)
  }

  def send(to: ActorId, msg: Message)(implicit network: Network): Unit = {
    network.send(self.id, to, msg)
  }
}

sealed abstract class Behavior

abstract class ExtensibleBehavior extends Behavior {
  def receiveMessage(ctx: ActorContext, msg: Message)(implicit network: Network): Behavior
}

object Behavior {
  case class Same()      extends Behavior
  case class Stopped()   extends Behavior
  case class Unhandled() extends Behavior

  def same: Behavior = Same()
  def stopped: Behavior = Stopped()
  def unhandled: Behavior = Unhandled()
}

object Actor {
  case class Immutable(onMessage: (ActorContext, Message, Network) => Behavior) extends ExtensibleBehavior {
    def receiveMessage(ctx: ActorContext, msg: Message)(implicit network: Network): Behavior =
      onMessage(ctx, msg, network)
  }

  def immutable(onMessage: (ActorContext, Message, Network) => Behavior): Behavior =
    Immutable(onMessage)

  case class Deferred(factory: (ActorContext, Network) => Behavior) extends Behavior {
    def apply(ctx: ActorContext)(implicit network: Network): Behavior = factory(ctx, network)
  }

  def deferred(factory: (ActorContext, Network) => Behavior): Behavior =
    Deferred(factory)

  def stopped: Behavior =
    Behavior.stopped

  def same: Behavior =
    Behavior.same

  def unhandled: Behavior =
    Behavior.unhandled
}
