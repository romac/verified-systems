
import stainless.lang._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object tvar {

  case class WaitMap(map: Map[BigInt, List[Process]]) {
    def apply(tvarId: BigInt): List[Process] = {
      if (map contains tvarId) map(tvarId)
      else Nil()
    }

    def updated(tvarId: BigInt, processes: List[Process]): WaitMap = {
      WaitMap(map.updated(tvarId, processes))
    }

    def waitOn(tvarId: BigInt, process: Process): WaitMap = {
      updated(tvarId, apply(tvarId) :+ process)
    }

    def unwaitOn(tvarId: BigInt, process: Process): WaitMap = {
      updated(tvarId, apply(tvarId) - process)
    }
  }

  object WaitMap {
    def empty: WaitMap = WaitMap(Map.empty[BigInt, List[Process]])
  }

  case class TVarMap(map: Map[BigInt, TVar]) {
    def apply(id: BigInt): TVar = {
      if (map contains id) map(id)
      else TVar(id, None())
    }

    def update(tvar: TVar): TVarMap = {
      TVarMap(map.updated(tvar.id, tvar))
    }
  }

  object TVarMap {
    def empty: TVarMap = TVarMap(Map.empty[BigInt, TVar])
  }

  sealed abstract class Op

  case class Take(process: Process, tvar: BigInt) extends Op
  case class Put(process: Process, tvar: BigInt, value: BigInt) extends Op

  case class TVar(id: BigInt, value: Option[BigInt]) {
    def isEmpty: Boolean  = !nonEmpty
    def nonEmpty: Boolean = value.isDefined

    def get: BigInt = {
      require(nonEmpty)
      value.get
    }

    def set(x: BigInt): TVar = {
      require(isEmpty)
      TVar(id, Some(x))
    }

    // def modify(f: BigInt => BigInt): TVar = {
    //   TVar(id, value.map(f))
    // }

    def empty: TVar = {
      require(nonEmpty)
      TVar(id, None())
    }
  }

  case class Process(id: BigInt) {

    def take(tvar: TVar): Op =
      Take(this, tvar.id)

    def put(tvar: TVar, value: BigInt): Op =
      Put(this, tvar.id, value)

  }

  sealed abstract class Event
  object Event {
    case class Put(pid: BigInt, tid: BigInt, value: BigInt)  extends Event
    case class Take(pid: BigInt, tid: BigInt, value: BigInt) extends Event
    case class Deadlock(processId: BigInt, tvarId: BigInt)   extends Event
  }

  case class System(
    tvars: TVarMap,
    waitMap: WaitMap,
    trace: List[Event],
    ops: List[Op]
  ) {

    def isDeadlocked: Boolean = {
      trace.nonEmpty && trace.last.isInstanceOf[Event.Deadlock]
    }

    def run: System = {
      val next = step
      if (next.isDeadlocked || next == this) next
      else next.run
    }

    def step: System = {
      ops match {
        case Nil() => this
        case op :: rest =>
          op match {
            case Take(process, tid) if tvars(tid).nonEmpty =>
              val waitingOn = waitMap(tid)
              val value     = tvars(tid).get

              System(
                tvars.update(tvars(tid).empty),
                waitMap.unwaitOn(tid, process),
                trace :+ Event.Take(process.id, tid, value),
                rest
              )

            case Take(process, tid) if tvars(tid).isEmpty =>
              val newTrace = if (rest.isEmpty) trace :+ Event.Deadlock(process.id, tid) else trace
              System(
                tvars,
                waitMap.waitOn(tid, process),
                newTrace,
                rest :+ op
              )

            case Put(process, tid, _) if tvars(tid).nonEmpty =>
              val newTrace = if (rest.isEmpty) trace :+ Event.Deadlock(process.id, tid) else trace
              System(
                tvars,
                waitMap.waitOn(tid, process),
                newTrace,
                rest :+ op
              )

            case Put(process, tid, value) if tvars(tid).isEmpty =>
              System(
                tvars.update(tvars(tid).set(value)),
                waitMap.unwaitOn(tid, process),
                trace :+ Event.Put(process.id, tid, value),
                rest
              )
          }
      }
    }
  }

  val a  = TVar(1, None())
  val b  = TVar(2, None())
  val p1 = Process(1)
  val p2 = Process(2)

  val p1Ops = List(
    p1.take(a)
  )

  val p2Ops = List(
    p2.put(a, 42),
    p2.take(a)
  )

  def zippers[A](list: List[A]): List[(List[A], A, List[A])] = {
    require(list.nonEmpty)

    def go(xs: List[A], l: List[A]): List[(List[A], A, List[A])] = {
      l match {
        case Nil() => Nil()
        case Cons(y, ys) => (xs, y, ys) :: go(y :: xs, ys)
      }
    }

    go(List(), list)
  } ensuring { _.nonEmpty }

  def interleavings[A](list: List[List[A]]): List[List[A]] = list match {
    case Nil() =>
      List(Nil())

    case xss =>
      zippers(xss) flatMap {
        case (_, Nil(), _) =>
          List(Nil())

        case (xssL, Cons(head, xs), xssR) =>
          val sub = if (xs.isEmpty) Nil[List[A]]() else List(xs)
          interleavings(sub ++ xssL ++ xssR) map (head :: _)
      }
  }

  @force
  val runs = interleavings(List(p1Ops, p2Ops))

  def test = {
    runs == List(
      List(p1.take(a), p2.put(a, 42), p2.take(a)),
      List(p2.put(a, 42), p2.take(a), p1.take(a)),
      List(p2.put(a, 42), p1.take(a), p2.take(a))
    )
  } holds

  def deadlocks(ops: List[Op]) = {
    val system = System(
      TVarMap(Map(a.id -> a)),
      WaitMap.empty,
      List(),
      ops
    )

    system.run.isDeadlocked
  }

  @force def run1 = deadlocks(runs(0)).holds
  @force def run2 = deadlocks(runs(1)).holds
  @force def run3 = deadlocks(runs(2)).holds

}
