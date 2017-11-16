
import stainless.lang._
import stainless.annotation._
import stainless.collection._

object inter {
  // just the identity function on List[List[A]]
  def foo[A](list: List[List[A]]): List[List[A]] = {
    list flatMap {
      case Nil() =>
        List(Nil())
      case Cons(head, tail) =>
        foo(List(tail)) map { rest => Cons(head, rest) }
    }
  }

  @force
  def test = foo(List(List(1), List(2), List(3)))
}

