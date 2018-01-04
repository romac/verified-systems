package crdt

import stainless.lang._
import stainless.collection._
import stainless.math.max

case class ClockMap(map: Map[ActorRef, Clock]) {

  def apply(k: ActorRef): Clock = {
    map.getOrElse(k, Clock.zero)
  }

  def updated(k: ActorRef, v: Clock): ClockMap = {
    ClockMap(map.updated(k, v))
  }

  def equalsForIds(to: ClockMap, ids: List[ActorRef]) = {
    ids.foldLeft(true)((acc, id) => acc && (this.apply(id) == to.apply(id)))
  }

}

object ClockMap {
  def empty: ClockMap = ClockMap(Map.empty[ActorRef, Clock])

  def merge(a: ClockMap, b: ClockMap, ids: List[ActorRef]): ClockMap = {
    require(ids.isDistinct)

    ids.foldLeft(ClockMap.empty) { case (acc, id) =>
      val left  = a(id)
      val right = b(id)
      acc.updated(id, left merge right)
    }
  }

}

object ClockMapTheorems {

  import ClockTheorems._

  def ClockMap_merge_idempotent(a: ClockMap, ids: List[ActorRef]): Boolean = {
    require(ids.nonEmpty && ids.isDistinct)
    assert(ClockTheorems.Clock_merge_semilattice)
    ClockMap.merge(a, a, ids) == a
  } holds

  def ClockMap_merge_assoc(a: ClockMap, b: ClockMap, c: ClockMap, ids: List[ActorRef]): Boolean = {
    require(ids.nonEmpty && ids.isDistinct)
    assert(ClockTheorems.Clock_merge_semilattice)
    ClockMap.merge(a, ClockMap.merge(b, c, ids), ids) == ClockMap.merge(ClockMap.merge(a, b, ids), c, ids)
  } holds

  def ClockMap_merge_commutative(a: ClockMap, b: ClockMap, ids: List[ActorRef]): Boolean = {
    require(ids.nonEmpty && ids.isDistinct)
    assert(ClockTheorems.Clock_merge_semilattice)
    ClockMap.merge(a, b, ids) == ClockMap.merge(b, a, ids)
  } holds

}

