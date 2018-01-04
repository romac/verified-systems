package crdt

import stainless.lang._
import stainless.collection._
import stainless.math.max

case class GCounter(shards: ClockMap, ids: List[ActorRef]) {

  require(ids.nonEmpty && ids.isDistinct)

  @inline
  def validContext(ctx: ActorContext): Boolean = {
    ctx.actors == ids && ids.contains(ctx.self) &&
    ids.isDistinct && ctx.actors.isDistinct
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
    ctx.actors.nonEmpty && ctx.actors.contains(ctx.self) && ctx.actors.isDistinct
  }

  def zero(implicit ctx: ActorContext): GCounter = {
    require(validContext(ctx))

    GCounter(ClockMap.empty, ctx.actors)
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
