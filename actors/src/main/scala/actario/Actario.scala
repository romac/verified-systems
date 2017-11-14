
package actario

import stainless.lang._
import stainless.annotation._
import stainless.collection._

sealed abstract class Name {
  @extern
  override def toString: String = this match {
    case Name.Toplevel(name) => name
    case Name.Generated(num, parent) => s"$parent($num)"
  }
}
object Name {
  final case class Toplevel(name: String)               extends Name
  final case class Generated(num: BigInt, parent: Name) extends Name
}

abstract class Msg
case class NotUsed() extends Msg

abstract class Behavior {
  def receive(msg: Msg): Action

  def same: Behavior = this
}

object Behavior {
  case object Stopped extends Behavior {
    def receive(msg: Msg): Action = Action.become(Stopped)
  }

  case object Unhandled extends Behavior {
    def receive(msg: Msg): Action = Action.become(Unhandled)
  }
}

sealed abstract class Action
object Action {
  final case class Spawn(behavior: Behavior, cont: Name => Action) extends Action
  final case class Send(to: Name, msg: Msg, next: Action)          extends Action
  final case class Self(cont: Name => Action)                      extends Action
  final case class Become(behavior: Behavior)                      extends Action

  @inline
  def spawn(behavior: Behavior)(cont: Name => Action): Action =
    Spawn(behavior, cont)

  @inline
  def send(to: Name, msg: Msg)(next: Action): Action =
    Send(to, msg, next)

  @inline
  def self(cont: Name => Action): Action =
    Self(cont)

  @inline
  def become(behavior: Behavior): Action =
    Become(behavior)

}

final case class Actor(
  name: Name,
  behavior: Behavior,
  nextNum: BigInt
) {

  def genName: (Name, Actor) = {
    val subName = Name.Generated(nextNum, name)
    val actor = Actor(name, behavior, nextNum + 1)
    (subName, actor)
  }

  def become(nextBehavior: Behavior): Actor = {
    Actor(name, nextBehavior, nextNum)
  }

}

final case class InFlight(
  to: Name,
  from: Name,
  msg: Msg
)

sealed abstract class Label {
  @extern
  override def toString: String = Label.asString(this)
}

object Label {
  final case class Receive(from: Name, to: Name, msg: Msg) extends Label
  final case class Send(from: Name, to: Name, msg: Msg)    extends Label
  final case class Spawn(name: Name)                       extends Label
  final case class Become(name: Name, behavior: Behavior)  extends Label
  final case class Self(me: Name)                          extends Label

  @extern
  def asString(l: Label): String = l match {
    case Receive(from, to: Name, msg: Msg) =>
      to + " <- " + msg + " <- " + from

    case Send(from, to: Name, msg: Msg) =>
      from + " -> " + msg + " -> " + to

    case Spawn(name) =>
      "\\o/ " + name

    case Become(name, behavior) =>
      name + " ./ " + behavior

    case Self(me) =>
      me + " ?"
  }
}

case class ActorContext(
  self: Actor,
  trace: List[Label]
) {

  def updateSelf(newSelf: Actor): ActorContext = {
    ActorContext(newSelf, trace)
  }

  def +(label: Label): ActorContext = {
    ActorContext(self, trace :+ label)
  }

}

final case class System(
  inFlight: List[InFlight],
  actors: List[Actor]
) {

  def parentOf(actor: Actor): Option[Actor] = actor.name match {
    case Name.Toplevel(_)              => None()
    case Name.Generated(_, parentName) => actors.find(_.name == parentName)
  }

  def actorExists(name: Name): Boolean = {
    actors.find(_.name == name).isDefined
  }

  def actorNamed(name: Name): Actor = {
    require(actorExists(name))
    actors.find(_.name == name).get
  }

  def updateActor(a: Actor): System = {
    require(actorExists(a.name))
    System(inFlight, actors.map { b =>
      if (b.name == a.name) a else b
    })
  }

  def addActor(a: Actor): System = {
    require(!actorExists(a.name))
    System(inFlight, Cons(a, actors))
  }

  def removeActor(a: Actor): System = {
    require(actorExists(a.name))
    System(inFlight, actors - a)
  }

  def addInFlight(inf: InFlight): System = {
    System(inFlight :+ inf, actors)
  }

  def send(from: Name, to: Name, msg: Msg): System = {
    addInFlight(InFlight(from, to, msg))
  }

  def removeInFlight(inf: InFlight): System = {
    System(inFlight - inf, actors)
  }

  def run(trace: List[Label]): (System, List[Label]) = {
    step(trace) match {
      case Some((next, nextTrace)) =>
        next.run(nextTrace)

      case _ =>
        (this, trace)

    }
  }

  def step(trace: List[Label]): Option[(System, List[Label])] = inFlight.headOption map {
    case inf @ InFlight(from, to, msg) =>
      val actor = actorNamed(to)
      val action = actor.behavior.receive(msg)
      val ctx = ActorContext(actor, List(Label.Receive(from, to, msg)))
      val (newSystem, newCtx) = eval(ctx, action)
      (newSystem.removeInFlight(inf), trace ++ newCtx.trace)
  }

  def eval(ctx: ActorContext, action: Action): (System, ActorContext) = action match {
    case Action.Self(cont) => 
      this.eval(
        ctx + Label.Self(ctx.self.name),
        cont(ctx.self.name)
      )

    case Action.Send(to, msg, next) =>
        this
          .send(ctx.self.name, to, msg)
          .eval(ctx + Label.Send(ctx.self.name, to, msg), next)

    case Action.Spawn(behavior, cont) =>
      val (name, next) = ctx.self.genName
      this
        .addActor(Actor(name, behavior, 1))
        .eval(ctx.updateSelf(next) + Label.Spawn(name), cont(name))

    case Action.Become(behavior) =>
      val next = this.updateActor(ctx.self.become(behavior))
      (next, ctx + Label.Become(ctx.self.name, behavior))
  }

}

object System {

  def initial(guardianBehavior: Behavior, name: String): System = {
    val system = System(List(), List())
    val guardianName = Name.Toplevel(name)
    val guardianActor = Actor(guardianName, guardianBehavior, 1)

    system
      .addActor(guardianActor)
      .send(guardianName, guardianName, NotUsed())
  }

}

  // def transitionTo(label: Label): System = label match {
  //   case Label.Receive(to, from, msg) =>
  //     require {
  //       actorExists(to) && actorExists(from)
  //       inFlight.contains(InFlight(to, from, msg))
  //     }

  //     val toActor = actorNamed(to)

  //     val Action.Become(behav) = toActor.nextAction
  //     val nextAction = behav.receive(msg)
  //     val nextActor = Actor(toActor.name, nextAction, toActor.nextNum)

  //     this.updateActor(nextActor)
  //         .removeInFlight(InFlight(to, from, msg))

  //   case Label.Send(from, to, msg) =>
  //     require(actorExists(to) && actorExists(from))

  //     val newInFlight = InFlight(to, from, msg)
  //     val fromActor = actorNamed(from)
  //     val nextActor = fromActor.nextAction match {
  //       case Action.Send(t, m, next) if t == to && m == msg =>
  //         Actor(fromActor.name, next, fromActor.nextNum)

  //       case _ => fromActor
  //     }

  //     this.addInFlight(newInFlight)
  //         .updateActor(nextActor)

  //   case Label.Spawn(name @ Name.Generated(n, parent)) =>
  //     require(actorExists(parent) && !actorExists(name))

  //     val parentActor = actorNamed(parent)
  //     parentActor.nextAction match {
  //       case Action.Spawn(behav, cont) =>
  //         val newActor = Actor(name, Action.Become(behav), 0)
  //         val nextActor = Actor(parentActor.name, cont(name), parentActor.nextNum + 1)
  //         this.addActor(newActor).updateActor(nextActor)

  //       case _ => this
  //     }

  //   case Label.Spawn(_) =>
  //     this // Impossible

  //   case Label.Self(me) =>
  //     require(actorExists(me))

  //     val meActor = actorNamed(me)
  //     val nextActor = meActor.nextAction match {
  //       case Action.Self(cont) =>
  //         Actor(meActor.name, cont(me), meActor.nextNum)

  //       case _ => meActor
  //     }

  //     this.updateActor(nextActor)
  // }

