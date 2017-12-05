package crdt

import stainless.lang._

case class TwoPSet[A](added: GSet[A], removed: GSet[A]) {

  def contains(x: A): Boolean =
    added.contains(x) && !removed.contains(x)

  def +(x: A): TwoPSet[A] =
    TwoPSet[A](added + x, removed)

  def remove(x: A): TwoPSet[A] = {
    require(contains(x))
    TwoPSet[A](added, removed + x)
  }

  def merge(that: TwoPSet[A]): TwoPSet[A] =
    TwoPSet[A](this.added ++ that.added, this.removed ++ that.removed)

}

object TwoPSetTheorems {

  def TwoPSet_merge_idempotent[A](g: TwoPSet[A]): Boolean = {
    g.merge(g) == g
  } holds

  def TwoPSet_merge_associative[A](f: TwoPSet[A], g: TwoPSet[A], h: TwoPSet[A]): Boolean = {
    f.merge(g.merge(h)) == f.merge(g).merge(h)
  } holds

  def TwoPSet_merge_commutative[A](g: TwoPSet[A], h: TwoPSet[A]): Boolean = {
    g.merge(h) == h.merge(g)
  } holds

}
