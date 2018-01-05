package crdt

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import stainless.math.max

sealed abstract class ClockMap {
  def apply(k: ActorRef): Clock
  def updated(k: ActorRef, v: Clock): ClockMap

  def ===(that: ClockMap): Boolean = forall { (k: ActorRef) =>
    this(k) == that(k)
  }

  def <=(that: ClockMap): Boolean = forall { (k: ActorRef) =>
    this(k) <= that(k)
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

  def ClockMap_merge_monotonic(c1: ClockMap, c2: ClockMap): Boolean = {
    val c3 = c1 merge c2
    c1 <= c3 && c2 <= c3
  } holds

  def ClockMap_compare_refl(c1: ClockMap): Boolean = {
    c1 <= c1
  } holds

  def ClockMap_merge_partialOrder: Boolean = {
    forall((c1: ClockMap) => ClockMap_compare_refl(c1))
    forall((c1: ClockMap, c2: ClockMap) => ClockMap_merge_monotonic(c1, c2))
  } holds

  def ClockMap_merge_semilattice: Boolean = {
    forall((c1: ClockMap, c2: ClockMap) => ClockMap_merge_commutative(c1, c2)) &&
    forall((c1: ClockMap) => ClockMap_merge_idempotent(c1)) &&
    forall((c1: ClockMap, c2: ClockMap, c3: ClockMap) => ClockMap_merge_associative(c1, c2, c3)) &&
    ClockMap_merge_partialOrder
  } holds

  @library
  def ClockMap_equality(a: ClockMap, b: ClockMap): Boolean = {
    require(a === b)
    a == b
  }

}

