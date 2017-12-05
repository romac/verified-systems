package crdt

import stainless.lang._

case class GSet[A](set: Set[A] = Set.empty[A]) {

  def contains(x: A): Boolean =
    set contains x

  def +(x: A): GSet[A] =
    GSet[A](set + x)

  def ++(that: GSet[A]): GSet[A] =
    merge(that)

  def merge(that: GSet[A]): GSet[A] =
    GSet[A](set ++ that.set)

}

object GSetTheorems {

  def GSet_merge_idempotent[A](g: GSet[A]): Boolean = {
    g.merge(g) == g
  } holds

  def GSet_merge_associative[A](g: GSet[A], h: GSet[A], f: GSet[A]): Boolean = {
    g.merge(h).merge(f) == g.merge(h.merge(f))
  } holds

  def GSet_merge_commutative[A](g: GSet[A], h: GSet[A]): Boolean = {
    g.merge(h) == h.merge(g)
  } holds

}
