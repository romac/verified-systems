
package actors

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object registry {

  case object Main extends ActorRef("main", None())
  case object Registry extends ActorRef("registry", None())

  object MainB {
    case object Init extends Msg
  }

  case class MainB(registry: ActorRef, initialized: Boolean) extends Behavior {
    import MainB._

    require(registry == Registry)

    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Init if !initialized =>
        registry ! RegistryB.Register(ctx.self)

        val w1 = ctx.spawn(WorkerB(false), "worker-1")
        val w2 = ctx.spawn(WorkerB(false), "worker-2")

        w1 ! WorkerB.Init(registry)
        w2 ! WorkerB.Init(registry)

        MainB(registry, true)

      case _ =>
        Behavior.same
    }
  }

  object WorkerB {
    case class Init(registry: ActorRef) extends Msg
  }

  case class WorkerB(registered: Boolean) extends Behavior {
    import WorkerB._

    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Init(registry) if !registered =>
        registry ! RegistryB.Register(ctx.self)
        WorkerB(true)

      case _ =>
        Behavior.same
    }
  }

  object RegistryB {
    case class Register(me: ActorRef) extends Msg
  }

  case class RegistryB(register: List[ActorRef]) extends Behavior {
    import RegistryB._

    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Register(ref) =>
        RegistryB(Cons(ref, register))

      case _ =>
        Behavior.same
    }
  }

  def isWorker(a: ActorRef)(s: ActorSystem): Boolean = {
    s.behaviors(a).isInstanceOf[WorkerB]
  }

  val w1 = Child("worker-1", Main)
  val w2 = Child("worker-2", Main)

  def validBehaviors(s: ActorSystem): Boolean = {
    s.behaviors(Main).isInstanceOf[MainB] &&
    s.behaviors(Registry).isInstanceOf[RegistryB] && {
      val MainB(reg, init) = s.behaviors(Main)
      if (!init) {
        s.behaviors(w1) == Behavior.stopped &&
        s.behaviors(w2) == Behavior.stopped
      } else {
        val RegistryB(reg) = s.behaviors(Registry)
        s.behaviors(w1).isInstanceOf[WorkerB] &&
        s.behaviors(w2).isInstanceOf[WorkerB] && {
          val WorkerB(w1Init) = s.behaviors(w1)
          val WorkerB(w2Init) = s.behaviors(w2)
          reg.contains(w1) == w1Init
          reg.contains(w2) == w2Init
        }
      }
    } &&
    forall { (ref: ActorRef) =>
      !(ref == Main || ref == Registry || ref == w1 || ref == w2) ==> (s.behaviors(ref) == Behavior.stopped)
    }
  }

  def noMsgToSelf(s: ActorSystem, self: ActorRef): Boolean = {
    s.inboxes(self -> self).isEmpty
  }

  def validMessages(s: ActorSystem): Boolean = {
    noMsgToSelf(s, Main) &&
    noMsgToSelf(s, Registry) &&
    noMsgToSelf(s, w1) &&
    noMsgToSelf(s, w1) &&
    (s.inboxes(Main -> Registry).forall {
      case RegistryB.Register(id) => id == Main
      case _ => false
    }) &&
    (s.inboxes(Main -> w1).forall {
      case WorkerB.Init(reg) => reg == Registry
      case _ => false
    }) &&
    (s.inboxes(Main -> w2).forall {
      case WorkerB.Init(reg) => reg == Registry
      case _ => false
    }) &&
    (s.inboxes(w1 -> Registry).forall {
      case RegistryB.Register(id) => id == w1
      case _ => false
    }) &&
    (s.inboxes(w2 -> Registry).forall {
      case RegistryB.Register(id) => id == w2
      case _ => false
    }) &&
    s.inboxes(Registry -> Main).isEmpty &&
    s.inboxes(Registry -> w1).isEmpty &&
    s.inboxes(Registry -> w2).isEmpty && 
    s.inboxes(w1 -> Main).isEmpty &&
    s.inboxes(w2 -> Main).isEmpty
  }

  def invariant(s: ActorSystem): Boolean = {
    validMessages(s) && validBehaviors(s)
  }

  def theorem(s: ActorSystem, from: ActorRef, to: ActorRef): Boolean = {
    require(invariant(s))
    assert(validBehaviors(s.step(Main, Main)))
    assert(validMessages(s.step(Main, Main)))
    assert(validBehaviors(s.step(Registry, Registry))) // timeout
    assert(validMessages(s.step(Registry, Registry)))  // timeout
    assert(validBehaviors(s.step(w1, w1)))             // timeout
    assert(validMessages(s.step(w1, w1)))              // timeout
    assert(validBehaviors(s.step(w2, w2)))             // timeout
    assert(validMessages(s.step(w2, w2)))              // timeout
    assert(validBehaviors(s.step(Main, Registry)))     // timeout
    assert(validMessages(s.step(Main, Registry)))      // timeout
    assert(validBehaviors(s.step(Main, w1)))           // timeout
    assert(validMessages(s.step(Main, w1)))            // timeout
    assert(validBehaviors(s.step(Main, w2)))           // timeout
    assert(validMessages(s.step(Main, w2)))            // timeout
    assert(validBehaviors(s.step(w1, Registry)))       // timeout
    assert(validMessages(s.step(w1, Registry)))        // timeout
    assert(validBehaviors(s.step(w2, Registry)))       // timeout
    assert(validMessages(s.step(w2, Registry)))        // timeout
    invariant(s.step(from, to))
  } holds

}
