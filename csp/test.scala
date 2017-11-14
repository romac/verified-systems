
import stainless.lang._
import stainless.annotation._
import stainless.collection._

object inter {

  def foo[A](list: List[List[A]]): List[List[A]] = {
    list flatMap {
      case Nil() =>
        List(Nil())
      case Cons(head, tail) =>
        foo(List(tail)) map { rest => Cons(head, rest) }
    }
  }

  val list: List[List[BigInt]] = List(List(1), List(2), List(3))

  @force
  def test = {
    foo(list)
  }

}

