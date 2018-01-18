package crdt

import stainless.lang._
import stainless.proof._
import stainless.annotation._
import stainless.collection._
import stainless.math.max

case class GCounter(shards: ClockMap) {

  @inline
  def +(n: BigInt)(implicit ctx: ActorContext): GCounter = {
    require(n >= 0)
    increment(n)
  }

  def increment(n: BigInt = 1)(implicit ctx: ActorContext): GCounter = {
    require(n >= 0)
    GCounter(shards.updated(ctx.self, shards(ctx.self) + n))
  }

  def value(implicit ctx: ActorContext): BigInt = {
    ctx.actors.map(id => shards(id).value).foldLeft(BigInt(0))(_ + _)
  }

  def merge(that: GCounter): GCounter = {
    GCounter(this.shards merge that.shards)
  }

  def <=(that: GCounter): Boolean = {
    this.shards <= that.shards
  }

  def ===(that: GCounter): Boolean = {
    this.shards === that.shards
  }
}

object GCounter {
  def validContext(ctx: ActorContext) = {
    ctx.actors.nonEmpty && ctx.actors.contains(ctx.self) && ctx.actors.isDistinct
  }

  def zero: GCounter = {
    GCounter(ClockMap.empty)
  }
}

object GCounterTheorems {

  import ClockTheorems._
  import ClockMapTheorems._

  def GCounter_merge_idempotent(a: GCounter): Boolean = {
    assert(ClockMap_merge_idempotent(a.shards))
    a.merge(a) === a
  } holds

  def GCounter_merge_associative(a: GCounter, b: GCounter, c: GCounter): Boolean = {
    assert(ClockMap_merge_associative(a.shards, b.shards, c.shards))
    a.merge(b).merge(c) === a.merge(b.merge(c))
  } holds

  def GCounter_merge_commutative(a: GCounter, b: GCounter): Boolean = {
    assert(ClockMap_merge_commutative(a.shards, b.shards))
    a.merge(b) === b.merge(a)
  } holds

  def GCounter_merge_monotonic(c1: GCounter, c2: GCounter): Boolean = {
    val c3 = c1 merge c2
    c1 <= c3 && c2 <= c3
  } holds

  def GCounter_compare_refl(c1: GCounter): Boolean = {
    c1 <= c1
  } holds

  def GCounter_merge_partialOrder: Boolean = {
    forall((c1: GCounter) => GCounter_compare_refl(c1))
    forall((c1: GCounter, c2: GCounter) => GCounter_merge_monotonic(c1, c2))
  } holds

  def GCounter_merge_semilattice: Boolean = {
    forall((c1: GCounter, c2: GCounter) => GCounter_merge_commutative(c1, c2)) &&
    forall((c1: GCounter) => GCounter_merge_idempotent(c1)) &&
    forall((c1: GCounter, c2: GCounter, c3: GCounter) => GCounter_merge_associative(c1, c2, c3)) &&
    GCounter_merge_partialOrder
  } holds

  // def GCounter_equals_sameValue(a: GCounter, b: GCounter)(implicit ctx: ActorContext): Boolean = {
  //   require(a === b && GCounter.validContext(ctx))

  //   val as = ctx.actors.map(id => a.shards(id).value)
  //   val ab = ctx.actors.map(id => b.shards(id).value)

  //   // assert(forall((id: ActorRef) => a.shards(id).value == b.shards(id).value))
  //   assert(as == ab)

  //   a.value == b.value
  // } holds

  // def GCounter_increment_homo_value(c: GCounter, n: BigInt)(implicit ctx: ActorContext): Boolean = {
  //   require(n >= 0 && GCounter.validContext(ctx))

  //   assert(ClockMap_merge_semilattice)
  //   ((c + n).value == c.value + n)
  // } holds

  // @library
  // def GCounter_equality(a: GCounter, b: GCounter): Boolean = {
  //   require(a === b)
  //   a == b
  // }

}

