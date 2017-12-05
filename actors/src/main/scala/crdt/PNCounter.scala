package crdt

import stainless.lang._
import stainless.proof._

case class PNCounter(pos: GCounter, neg: GCounter) {

  require(pos.ids == neg.ids)

  val ids = pos.ids

  @inline
  def validContext(implicit ctx: ActorContext): Boolean = {
    ctx.actors == ids && ids.contains(ctx.self) &&
    ids.hasNoDuplicates && ctx.actors.hasNoDuplicates
  }

  def increment(implicit ctx: ActorContext): PNCounter = {
    require(validContext)
    PNCounter(pos + 1, neg)
  }

  def decrement(implicit ctx: ActorContext): PNCounter = {
    require(validContext)
    PNCounter(pos, neg + 1)
  }

  def value: BigInt = {
    pos.value - neg.value
  }

  def merge(that: PNCounter): PNCounter = {
    require(this.ids == that.ids)

    PNCounter(pos merge that.pos, neg merge that.neg)
  }

  def equals(that: PNCounter): Boolean = {
    require(this.ids == that.ids)

    this.value == that.value
  }

}

object PNCounterTheorems {

  import GCounterTheorems._

  def PNCounter_merge_idempotent(g: PNCounter): Boolean = {
    (g.merge(g) equals g) because {
      GCounter_merge_idempotent(g.pos) &&
      GCounter_merge_idempotent(g.neg)
    }
  } holds

  def PNCounter_merge_associative(g: PNCounter, h: PNCounter, f: PNCounter): Boolean = {
    require(g.ids == h.ids && h.ids == f.ids)

    (g.merge(h).merge(f) equals g.merge(h.merge(f))) because {
      GCounter_merge_associative(g.pos, h.pos, f.pos) &&
      GCounter_merge_associative(g.neg, h.neg, f.neg)
    }
  } holds

  def PNCounter_merge_commutative(g: PNCounter, h: PNCounter): Boolean = {
    require(g.ids == h.ids)

    (g.merge(h) equals h.merge(g)) because {
      GCounter_merge_commutative(g.pos, h.pos) &&
      GCounter_merge_commutative(g.neg, h.neg)
    }
  } holds

}
