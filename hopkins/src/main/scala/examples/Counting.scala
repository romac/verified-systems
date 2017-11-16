
package hopkins

import stainless.lang._
import stainless.lang.utils._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object counting {

  case class PrimBehav(counter: Counter) extends Behavior {

    override
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Inc() =>
        Backup() ! Deliver(counter.increment)
        PrimBehav(counter.increment)

      case _ => Behavior.same
    }
  }

  case class BackBehav(counter: Counter) extends Behavior {

    override
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Deliver(c) => BackBehav(c)
      case _ => Behavior.same
    }


  }

  case class Primary() extends ActorId
  case class Backup()  extends ActorId

  case class Inc() extends Msg
  case class Deliver(c: Counter) extends Msg

  case class Counter(value: BigInt) {
    require(value >= 0)

    @inline
    def increment: Counter =
      Counter(value + 1)

    @inline
    def <=(that: Counter): Boolean = {
      this.value <= that.value
    }
  }

  def isSorted(list: List[Msg]): Boolean = list match {
    case Cons(Deliver(Counter(a)), rest@Cons(Deliver(Counter(b)), xs)) => a < b && isSorted(rest)
    case _ => true
  }

  def invariant(s: ActorSystem): Boolean = {
    s.inboxes((Backup(), Backup())).isEmpty && {
      (s.behaviors(Primary()), s.behaviors(Backup())) match {
        case (PrimBehav(p), BackBehav(b)) =>
          val bInbox = s.inboxes((Primary(), Backup()))
          p.value >= b.value && isSorted(bInbox) && bInbox.forall {
            case Deliver(Counter(i)) => p.value > i
            case _ => true
          }

          case _ => false
      }
    }
  }

  def theorem(s: ActorSystem, from: ActorId, to: ActorId): Boolean = {
    require(invariant(s))
    val newSystem = s.step(from, to)
    invariant(newSystem)
  } holds

  ////////////////
  // THEOREMS
  ////////////////

  /*

  @inline
  def backupValid(system: ActorSystem): Boolean = {
    isBackupBehavior(system.behaviors(Backup()))
  }

  @inline
  def primaryValid(system: ActorSystem): Boolean = {
    isPrimaryBehavior(system.behaviors(Primary()))
  }

  def isBackupBehavior(behavior: Behavior): Boolean = {
    behavior match {
      case BackBehav(_) => true
      case _ => false
    }
  }

  def isPrimaryBehavior(behavior: Behavior): Boolean = {
    behavior match {
      case PrimBehav(_) => true
      case _ => false
    }
  }

  def validBehaviors(system: ActorSystem): Boolean = {
    backupValid(system) && primaryValid(system)
  }

  def validMessages(system: ActorSystem): Boolean = {
    system.inFlightMsgs.forall(validMessage(_))
  }

  def validMessage(p: Packet): Boolean = p match {
    case Packet(Primary(), Primary.Inc()) => true
    case Packet(Primary(), Primary.Ack()) => true
    case Packet(Backup(), Backup.Inc())   => true
    case _                                => false
  }

  def validSystem(system: ActorSystem): Boolean = {
    validBehaviors(system) && validMessages(system)
  }

  def validSystem_initial: Boolean = {
    validSystem(ActorSystem.initial)
  } holds

  def validSystem_step(system: ActorSystem): Boolean = {
    require(isReachable(system) && validSystem(system))

    validSystem(system.step) because {
      validBehaviors_step(system) &&
      validMessages_step(system)
    }
  } holds

  def backupBehaviorStaysSame(behavior: Behavior, msg: Msg): Boolean = {
    require(isBackupBehavior(behavior))
    val backup = behavior.asInstanceOf[BackBehav]
    val ctx = ActorContext(Backup(), Nil())
    isBackupBehavior(backup.processMsg(msg)(ctx))
  } holds

  def primaryBehaviorStaysSame(behavior: Behavior, msg: Msg): Boolean = {
    require(isPrimaryBehavior(behavior))
    val primary = behavior.asInstanceOf[PrimBehav]
    val ctx = ActorContext(Primary(), Nil())
    isPrimaryBehavior(primary.processMsg(msg)(ctx))
  } holds

  def backupStaysBackup(system: ActorSystem, msg: Msg): Boolean = {
    require(isReachable(system) && validBehaviors(system))

    isBackupBehavior(system.deliverMessage(Backup(), msg)._1) because {
      backupBehaviorStaysSame(system.behaviors(Backup()), msg)
    }
  } holds

  def primaryStaysPrimary(system: ActorSystem, msg: Msg): Boolean = {
    require(isReachable(system) && validBehaviors(system))

    isPrimaryBehavior(system.deliverMessage(Primary(), msg)._1) because {
      primaryBehaviorStaysSame(system.behaviors(Primary()), msg)
    }
  } holds

  def validBehaviors_step(system: ActorSystem): Boolean = {
    require(isReachable(system) && validSystem(system))

    validBehaviors(system.step) because {
      if (system.inFlightMsgs.isEmpty) {
        noMsgsSameSystem(system)
      }
      else {
        system.inFlightMsgs.head match {
          case Packet(Backup(), msg) =>
            assert(backupStaysBackup(system, msg))

            isBackupBehavior(system.step.behaviors(Backup())) &&
            isPrimaryBehavior(system.step.behaviors(Primary()))

          case Packet(Primary(), msg) =>
            assert(primaryStaysPrimary(system, msg))

            isBackupBehavior(system.step.behaviors(Backup())) &&
            isPrimaryBehavior(system.step.behaviors(Primary()))
        }
      }
    }
  } holds

  def validMessages_step(system: ActorSystem): Boolean = {
    require(isReachable(system) && validSystem(system))

    validMessages(system.step) because {
      if (system.inFlightMsgs.isEmpty) {
        noMsgsSameSystem(system)
      }
      else {
        system.inFlightMsgs.head match {
          case Packet(Backup(), msg) =>
            assert(processEqualsDeliver(system, system.inFlightMsgs.head))
            assert(stepSendsFirstMsg(system, system.inFlightMsgs.head))
            assert(system.inFlightMsgs.tail.forall(validMessage(_)))

            val backup = system.behaviors(Backup())
            backupOnIncSendPrimaryAck(backup) &&
            backupIgnoreOtherMsgs(backup)

          case Packet(Primary(), msg) =>
            assert(processEqualsDeliver(system, system.inFlightMsgs.head))
            assert(stepSendsFirstMsg(system, system.inFlightMsgs.head))
            assert(system.inFlightMsgs.tail.forall(validMessage(_)))

            val primary = system.behaviors(Primary())
            primaryOnIncSendBackupInc(primary) &&
            primaryOnAckSendNoMsg(primary) &&
            primaryIgnoreOtherMsgs(primary)
        }
      }
    }
  } holds

  def primaryIgnoreOtherMsgs(behavior: Behavior): Boolean = {
    require(isPrimaryBehavior(behavior))

    val PrimBehav(before) = behavior
    val ctx = ActorContext(Primary(), Nil())
    val PrimBehav(after) = behavior.processMsg(Backup.Inc())(ctx)
    ctx.toSend.isEmpty && before == after
  } holds

  def primaryOnIncSendBackupInc(behavior: Behavior): Boolean = {
    require(isPrimaryBehavior(behavior))

    val PrimBehav(before) = behavior
    val ctx = ActorContext(Primary(), Nil())
    val PrimBehav(after) = behavior.asInstanceOf[PrimBehav].processMsg(Primary.Inc())(ctx)
    ctx.toSend == List(Packet(Backup(), Backup.Inc())) && before.increment == after
  } holds

  def primaryOnAckSendNoMsg(behavior: Behavior): Boolean = {
    require(isPrimaryBehavior(behavior))

    val PrimBehav(before) = behavior
    val ctx = ActorContext(Primary(), Nil())
    val PrimBehav(after) = behavior.asInstanceOf[PrimBehav].processMsg(Primary.Ack())(ctx)
    ctx.toSend == Nil[Packet]() && before == after
  } holds

  def backupIgnoreOtherMsgs(behavior: Behavior): Boolean = {
    require(isBackupBehavior(behavior))

    val ctx = ActorContext(Backup(), Nil())
    val afterInc = behavior.processMsg(Primary.Inc())(ctx)
    val inc = ctx.toSend.isEmpty && behavior.asInstanceOf[BackBehav] == afterInc

    val ctx2 = ActorContext(Backup(), Nil())
    val afterAck = behavior.processMsg(Primary.Ack())(ctx2)
    val ack = ctx2.toSend.isEmpty && behavior.asInstanceOf[BackBehav] == afterAck

    inc && ack
  } holds

  def backupOnIncSendPrimaryAck(behavior: Behavior): Boolean = {
    require(isBackupBehavior(behavior))

    val BackBehav(before) = behavior
    val ctx = ActorContext(Backup(), Nil())
    val BackBehav(after) = behavior.asInstanceOf[BackBehav].processMsg(Backup.Inc())(ctx)
    ctx.toSend == List(Packet(Primary(), Primary.Ack())) && before.increment == after
  } holds

  def prop_backupLessThanPrimary(system: ActorSystem): Boolean = {
    require(isReachable(system) && validSystem(system))

    val BackBehav(b) = system.behaviors(Backup())
    val PrimBehav(p) = system.behaviors(Primary())

    b <= p
  }

  def countIncMsgsToBackup(packets: List[Packet]): BigInt = {
    val incMsgs = packets.filter { p =>
      p.dest == Backup() && p.payload == Backup.Inc()
    }

    incMsgs.size
  } ensuring { _ >= 0 }

  def prop_backupPlusCountEqPrimary(system: ActorSystem): Boolean = {
    require(isReachable(system) && validSystem(system))

    val BackBehav(b) = system.behaviors(Backup())
    val PrimBehav(p) = system.behaviors(Primary())

    val diff = countIncMsgsToBackup(system.inFlightMsgs)
    b.value + diff == p.value
  }

  def backupPlusCountEqPrimary_initial: Boolean = {
    prop_backupPlusCountEqPrimary(ActorSystem.initial)
  } holds

  def backupPlusCountEqPrimary_step(system: ActorSystem): Boolean = {
    require(isReachable(system) && validSystem(system) && prop_backupPlusCountEqPrimary(system))

    assert(stepPreservesReachability(system) && isReachable(system.step))
    assert(validSystem_step(system) && validSystem(system.step))

    prop_backupPlusCountEqPrimary(system.step)
  } holds

  def backupPlusCountEqPrimary_implies_backupLessThanPrimary(system: ActorSystem): Boolean = {
    require(isReachable(system) && validSystem(system) && prop_backupPlusCountEqPrimary(system))

    prop_backupLessThanPrimary(system)
  } holds

  def thm_backupLessThanPrimary_initial: Boolean = {
    prop_backupLessThanPrimary(ActorSystem.initial) because {
      backupPlusCountEqPrimary_initial &&
      backupPlusCountEqPrimary_implies_backupLessThanPrimary(ActorSystem.initial)
    }
  } holds

  def thm_backupLessThanPrimary_step(system: ActorSystem): Boolean = {
    require(isReachable(system) && validSystem(system) && prop_backupLessThanPrimary(system))

    prop_backupLessThanPrimary(system.step) because {
      backupPlusCountEqPrimary_step(system) &&
      backupPlusCountEqPrimary_implies_backupLessThanPrimary(system.step)
    }
  } holds

  */

}
