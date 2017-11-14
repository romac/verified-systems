
import stainless.lang._
import stainless.annotation._
import stainless.collection._

object inter {

  def zippers[A](list: List[A]): List[(List[A], A, List[A])] = {
    require(list.nonEmpty)

    def go(xs: List[A], l: List[A]): List[(List[A], A, List[A])] = {
      l match {
        case Nil() => Nil()
        case Cons(y, ys) => (xs, y, ys) :: go(y :: xs, ys)
      }
    }

    go(List(), list)
  } ensuring { _.nonEmpty }

  def interleavings[A](list: List[List[A]]): List[List[A]] = list match {
    case Nil() =>
      List(Nil())

    case xss =>
      zippers(xss) flatMap {
        case (_, Nil(), _) =>
          List(Nil())

        case (xssL, Cons(head, xs), xssR) =>
          val sub = if (xs.isEmpty) Nil[List[A]]() else List(xs)
          interleavings(sub ++ xssL ++ xssR) map (head :: _)
      }
  }

  val list: List[List[BigInt]] = List(List(1), List(2), List(3))

  @force
  val zipped = zippers(list)

  @force
  val interleaved = interleavings(list)

  def zippedOk = {
    zipped == List[(List[List[BigInt]], List[BigInt], List[List[BigInt]])](
      (List(),List(1),List(List(2), List(3))),
      (List(List(1)),List(2),List(List(3))),
      (List(List(2), List(1)),List(3),List())
    )
  } holds

  def interleavedOk = {
    interleaved == List[List[BigInt]](
      List(1, 2, 3),
      List(1, 3, 2),
      List(2, 1, 3),
      List(2, 3, 1),
      List(3, 2, 1),
      List(3, 1, 2)
    )
  } holds

}
