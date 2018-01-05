package crdt

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import stainless.math.max

abstract class ClockMap {
  def apply(k: ActorRef): Clock
  def updated(k: ActorRef, v: Clock): ClockMap

  def ===(that: ClockMap): Boolean = forall { (k: ActorRef) =>
    this(k) == that(k)
  }

  def merge(that: ClockMap): ClockMap = {
    MergedClockMap(this, that)
  }
}

case class SingleClockMap(map: Map[ActorRef, Clock]) extends ClockMap {
  def apply(k: ActorRef): Clock = {
    map.getOrElse(k, Clock.zero)
  }

  def updated(k: ActorRef, v: Clock): ClockMap = {
    SingleClockMap(map.updated(k, v))
  }
}

case class MergedClockMap(left: ClockMap, right: ClockMap) extends ClockMap {
  def apply(k: ActorRef): Clock = {
    left(k) merge right(k)
  }

  def updated(k: ActorRef, v: Clock): ClockMap = {
    MergedClockMap(left.updated(k, v), right.updated(k, v))
  }
}

object ClockMap {
  def empty: ClockMap = SingleClockMap(Map.empty[ActorRef, Clock])
}

object ClockMapTheorems {

  import ClockTheorems._

  def ClockMap_merge_idempotent(a: ClockMap): Boolean = {
    assert(ClockTheorems.Clock_merge_semilattice)
    a.merge(a) === a
  } holds

  def ClockMap_merge_associative(a: ClockMap, b: ClockMap, c: ClockMap): Boolean = {
    assert(ClockTheorems.Clock_merge_semilattice)
    a.merge(b.merge(c)) === a.merge(b).merge(c)
  } holds

  def ClockMap_merge_commutative(a: ClockMap, b: ClockMap): Boolean = {
    assert(ClockTheorems.Clock_merge_semilattice)
    a.merge(b) === b.merge(a)
  } holds

  @library
  def ClockMap_equality(a: ClockMap, b: ClockMap): Boolean = {
    require(a === b)
    a == b
  }

}

