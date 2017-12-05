package crdt

object semilattice {

  def isIdempotent[A](op: (A, A) => A, a: A): Boolean = {
    op(a, a) == a
  }

  def isCommutative[A](op: (A, A) => A, a: A, b: A): Boolean = {
    op(a, b) == op(b, a)
  }

  def isAssociative[A](op: (A, A) => A, a: A, b: A, c: A): Boolean = {
    op(op(a, b), c) == op(a, op(b, c))
  }

}
