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
  @inline def zero: Clock = Clock(0)
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
    forall((c1: Clock, c2: Clock)            => Clock_merge_commutative(c1, c2))     &&
    forall((c1: Clock)                       => Clock_merge_idempotent(c1))          &&
    forall((c1: Clock, c2: Clock, c3: Clock) => Clock_merge_associative(c1, c2, c3)) &&
    Clock_merge_partialOrder
  } holds

}

