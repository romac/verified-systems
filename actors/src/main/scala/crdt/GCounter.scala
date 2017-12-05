package crdt

import stainless.lang._
import stainless.collection._
import stainless.math.max

case class Clock(value: BigInt) {

  require(value >= 0)

  @inline
  def +(n: BigInt): Clock = {
    require(n >= 0)
    Clock(value + n)
  }

  @inline
  def merge(that: Clock): Clock = {
    Clock(max(this.value, that.value))
  }

  @inline
  def <=(that: Clock): Boolean = {
    this.value <= that.value
  }

  @inline
  def <(that: Clock): Boolean = {
    this.value <= that.value
  }

}

object Clock {

  @inline
  def zero: Clock = Clock(0)

}

case class GCounter(shards: ClockMap, ids: List[ActorRef]) {

  require(ids.nonEmpty && ids.hasNoDuplicates)

  @inline
  def validContext(ctx: ActorContext): Boolean = {
    ctx.actors == ids && ids.contains(ctx.self) &&
    ids.hasNoDuplicates && ctx.actors.hasNoDuplicates
  }

  @inline
  def +(n: BigInt)(implicit ctx: ActorContext): GCounter = {
    require(n >= 0 && validContext(ctx))

    increment(n)
  }

  def increment(n: BigInt = 1)(implicit ctx: ActorContext): GCounter = {
    require(n >= 0 && validContext(ctx))

    val newClock = shards(ctx.self) + n
    GCounter(shards.updated(ctx.self, newClock), ids)
  }

  def value: BigInt = {
    ids.map(id => shards(id).value).foldLeft(BigInt(0))((_ + _))
  }

  def merge(that: GCounter): GCounter = {
    require(this.ids == that.ids)

    val newShards = ClockMap.merge(this.shards, that.shards, ids)
    GCounter(newShards, ids)
  } ensuring { res =>
    res.ids == this.ids
  }

  def compare(that: GCounter): Boolean = {
    require(this.ids == that.ids)

    val smaller = ids.map { id =>
      this.shards(id) <= that.shards(id)
    }
    smaller.exists(x => x)
  }

  def equals(that: GCounter): Boolean = {
    require(this.ids == that.ids)

    this.value == that.value
    // this.shards.equalsForIds(that.shards, ids)
  }
}

object GCounter {

  import GCounterTheorems._

  def validContext(ctx: ActorContext) = {
    ctx.actors.nonEmpty && ctx.actors.contains(ctx.self) && ctx.actors.hasNoDuplicates
  }

  def zero(implicit ctx: ActorContext): GCounter = {
    require(validContext(ctx))

    GCounter(ClockMap.empty, ctx.actors)
  }

}

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
    require(ids.hasNoDuplicates)

    ids.foldLeft(ClockMap.empty) { case (acc, id) =>
      val left  = a(id)
      val right = b(id)
      acc.updated(id, left merge right)
    }
  }

}

object MaxTheorems {

  def max_commutative(a: BigInt, b: BigInt): Boolean = {
    max(a, b) == max(b, a)
  }

  def max_idempotent(a: BigInt): Boolean = {
    max(a, a) == a
  }

  def max_associative(a: BigInt, b: BigInt, c: BigInt): Boolean = {
    max(max(a, b), c) == max(a, max(b, c))
  }

  def max_monotonic(a: BigInt, b: BigInt): Boolean = {
    val c = max(a, b)
    a <= c && b <= c
  }

  def max_semilattice: Boolean = {
    forall((a: BigInt, b: BigInt)            => max_commutative(a, b)) &&
    forall((a: BigInt)                       => max_idempotent(a)) &&
    forall((a: BigInt, b: BigInt, c: BigInt) => max_associative(a, b, c)) &&
    forall((a: BigInt, b: BigInt)            => max_monotonic(a, b))
  } holds

}

object ClockTheorems {

  def Clock_merge_commutative(c1: Clock, c2: Clock): Boolean = {
    c1.merge(c2) == c2.merge(c1)
  } holds

  def Clock_merge_idempotent(c1: Clock): Boolean = {
    c1.merge(c1) == c1
  } holds

  def Clock_merge_associative(c1: Clock, c2: Clock, c3: Clock): Boolean = {
    c1.merge(c2).merge(c3) == c1.merge(c2.merge(c3))
  } holds

  def Clock_merge_monotonic(c1: Clock, c2: Clock): Boolean = {
    val c3 = c1 merge c2

    c1 <= c3 && c2 <= c3
  } holds

  def Clock_compare_refl(c1: Clock): Boolean = {
    c1 <= c1
  } holds

  def Clock_merge_partialOrder: Boolean = {
    forall((c1: Clock)            => Clock_compare_refl(c1))
    forall((c1: Clock, c2: Clock) => Clock_merge_monotonic(c1, c2))
  } holds

  def Clock_merge_semilattice: Boolean = {
    forall((c1: Clock, c2: Clock)            => Clock_merge_commutative(c1, c2)) &&
    forall((c1: Clock)                       => Clock_merge_idempotent(c1)) &&
    forall((c1: Clock, c2: Clock, c3: Clock) => Clock_merge_associative(c1, c2, c3)) &&
    Clock_merge_partialOrder
  } holds

}

object ClockMapTheorems {

  import ClockTheorems._

  def ClockMap_merge_idempotent(a: ClockMap, ids: List[ActorRef]): Boolean = {
    require(ids.nonEmpty && ids.hasNoDuplicates)
    assert(ClockTheorems.Clock_merge_semilattice)
    ClockMap.merge(a, a, ids).equalsForIds(a, ids)
  } holds

  def ClockMap_merge_assoc(a: ClockMap, b: ClockMap, c: ClockMap, ids: List[ActorRef]): Boolean = {
    require(ids.nonEmpty && ids.hasNoDuplicates)
    assert(ClockTheorems.Clock_merge_semilattice)
    ClockMap.merge(a, ClockMap.merge(b, c, ids), ids) == ClockMap.merge(ClockMap.merge(a, b, ids), c, ids)
  } holds

  def ClockMap_merge_commutative(a: ClockMap, b: ClockMap, ids: List[ActorRef]): Boolean = {
    require(ids.nonEmpty && ids.hasNoDuplicates)
    assert(ClockTheorems.Clock_merge_semilattice)
    ClockMap.merge(a, b, ids) == ClockMap.merge(b, a, ids)
  } holds

}

object GCounterTheorems {

  import ClockTheorems._
  import ClockMapTheorems._

  def GCounter_increment_homo_value(c: GCounter, n: BigInt)(implicit ctx: ActorContext): Boolean = {
    require(n >= 0 && c.validContext(ctx))

    assert(Clock_merge_semilattice)

    (c + n).value == c.value + n
  } holds

  def GCounter_merge_idempotent(a: GCounter): Boolean = {
    assert(ClockMap_merge_idempotent(a.shards, a.ids))
    a.merge(a) equals a
  } holds

  def GCounter_merge_associative(a: GCounter, b: GCounter, c: GCounter): Boolean = {
    require(a.ids == b.ids && b.ids == c.ids)

    assert(ClockMap_merge_assoc(a.shards, b.shards, c.shards, a.ids))
    a.merge(b).merge(c) equals a.merge(b.merge(c))
  } holds

  def GCounter_merge_commutative(a: GCounter, b: GCounter): Boolean = {
    require(a.ids == b.ids)

    assert(ClockMap_merge_commutative(a.shards, b.shards, a.ids))
    a.merge(b) equals b.merge(a)
  } holds

  def GCounter_equals_sameValue(a: GCounter, b: GCounter): Boolean = {
    require(a.ids == b.ids && a.equals(b))

    a.value == b.value
  } holds

}
