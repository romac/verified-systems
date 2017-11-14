
import stainless.lang._
import stainless.lang.utils._
import stainless.data._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

package object hopkins {

  abstract class Msg

  case class Packet(dest: ActorId, payload: Msg)

  case class ActorContext(self: ActorId, var toSend: List[Packet]) {
    def send(to: ActorId, msg: Msg): Unit = {
      toSend = toSend :+ Packet(to, msg)
    }
  }

  abstract class ActorId {
    def !(msg: Msg)(implicit ctx: ActorContext): Unit = {
      ctx.send(this, msg)
    }
  }

  abstract class Behavior {

    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior

    @inline
    implicit def sameBehavior: Behavior = this
  }

  object Behavior {
    case class Stopped() extends Behavior {
      def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = {
          Behavior.same
        }
    }

    @inline
    def same(implicit behavior: Behavior): Behavior =
      behavior

    @inline
    def stopped: Stopped =
      Stopped()
  }

  case class ActorSystem(
    behaviors: CMap[ActorId, Behavior],
    inFlightMsgs: List[Packet]
  ) {

    @inline
    def hasInFlightMsgs: Boolean = {
      inFlightMsgs.nonEmpty
    }

    @extern
    def run: ActorSystem = {
      val next = step
      if (!next.hasInFlightMsgs) next
      else next.run
    }

    def step: ActorSystem = {
      inFlightMsgs match {
        case Nil() =>
          ActorSystem(behaviors, Nil())

        case packet :: rest =>
          // println(s"${packet.payload} -> ${packet.dest}")
          val (newBehavior, toSend) = deliverMessage(packet.dest, packet.payload)
          // println(s"${packet.dest} ./ $newBehavior")
          // println(s"${packet.dest} :: $toSend")

          ActorSystem(
            behaviors.updated(packet.dest, newBehavior),
            rest ++ toSend
          )
      }
    }

    def deliverMessage(actor: ActorId, msg: Msg): (Behavior, List[Packet]) = {
      val ctx      = ActorContext(actor, Nil())
      val behavior = behaviors(actor)

      val nextBehavior = behavior.processMsg(msg)(ctx)
      (nextBehavior, ctx.toSend)
    }

    def isStopped(id: ActorId): Boolean = {
      behaviors(id) == Behavior.stopped
    }

    def !(p: (ActorId, Msg)): ActorSystem = {
      val (id, msg) = p
      ActorSystem(behaviors, Packet(id, msg) :: inFlightMsgs).run
    }
  }

  object ActorSystem {
    @library
    def initial: ActorSystem = {
      val behaviors = CMap[ActorId, Behavior](initialBehavior.forActor(_))
      ActorSystem(behaviors, Nil())
    }
  }

  @library
  def isReachableFrom(system: ActorSystem, from: ActorSystem, steps: BigInt): Boolean = {
    if (system == from) true
    else if (steps > 0) isReachableFrom(system, from.step, steps - 1)
    else false
  }

  @library
  def isReachable(system: ActorSystem): Boolean = exists { (steps: BigInt) =>
    isReachableFrom(system, ActorSystem.initial, steps)
  }

  def initialSystemIsReachable: Boolean = {
    isReachable(ActorSystem.initial)
  } holds

  // FIXME: Currently unable to prove this, even if it literally follows from the definition of 'step'.
  @library
  def stepPreservesReachability(system: ActorSystem): Boolean = {
    require(isReachable(system))

    isReachableFrom(system.step, system, 1) && isReachable(system.step)
  } holds

  def stoppedActorsStayStopped(system: ActorSystem, actor: ActorId): Boolean = {
    require(isReachable(system) && system.isStopped(actor))

    system.step.isStopped(actor)
  } holds

  def stoppedActorsNeverSendMsgs(system: ActorSystem, actor: ActorId): Boolean = {
    require {
      isReachable(system) &&
      system.isStopped(actor) &&
      system.inFlightMsgs.nonEmpty &&
      system.inFlightMsgs.head.dest == actor
    }

    system.step.inFlightMsgs == system.inFlightMsgs.tail
  } holds

  def noMsgsSameSystem(system: ActorSystem): Boolean = {
    require(isReachable(system) && system.inFlightMsgs.isEmpty)

    system.step == ActorSystem(system.behaviors, system.inFlightMsgs)
  } holds

  def stepPreservesInvariantIfNoMsgs(system: ActorSystem, invariant: ActorSystem => Boolean): Boolean = {
    require(isReachable(system) && !system.hasInFlightMsgs && invariant(system))

    invariant(system.step)
  } holds

  def stepSendsFirstMsg(system: ActorSystem, packet: Packet): Boolean = {
    require {
      isReachable(system) &&
      system.hasInFlightMsgs &&
      system.inFlightMsgs.head == packet
    }

    val (_, sent) = system.deliverMessage(packet.dest, packet.payload)
    system.step.inFlightMsgs == system.inFlightMsgs.tail ++ sent
  } holds

  def processEqualsDeliver(system: ActorSystem, packet: Packet): Boolean = {
    require(isReachable(system))

    val behavior = system.behaviors(packet.dest)
    val ctx = ActorContext(packet.dest, Nil())
    behavior.processMsg(packet.payload)(ctx)

    val (_, sent) = system.deliverMessage(packet.dest, packet.payload)
    sent == ctx.toSend
  } holds

  def actorsDontChangeBehavSpontan(system: ActorSystem, id: ActorId): Boolean = {
    require {
      isReachable(system) &&
      system.behaviors.contains(id) &&
      system.inFlightMsgs.nonEmpty &&
      system.inFlightMsgs.head.dest != id
    }

    system.step.behaviors(id) == system.behaviors(id)
  } holds

  // @inline
  // def prop_previousSystem(system: ActorSystem, prev: ActorSystem): Boolean = {
  //   isReachable(prev) && prev.step == system
  // }

  // @library
  // def isReachableExistsPrev(system: ActorSystem): Boolean = {
  //   require(isReachable(system) && system != ActorSystem.initial)

  //   exists { (prev: ActorSystem) =>
  //     prop_previousSystem(system, prev)
  //   }
  // } holds

  // def previousSystem(system: ActorSystem): ActorSystem = {
  //   require(isReachable(system) && system != ActorSystem.initial)

  //   assert(isReachableExistsPrev(system))

  //   choose { (prev: ActorSystem) =>
  //     prop_previousSystem(system, prev)
  //   }
  // }

  def invariantHoldsAll(invariant: ActorSystem => Boolean) = {
    invariant(ActorSystem.initial) && forall { (system: ActorSystem) =>
      require(invariant(system))
      invariant(system.step)
    }
  }

}

