
import stainless.lang._
import stainless.lang.eval.force
import stainless.collection._

object inter {

  def zippers[A](list: List[A]): List[(List[A], A, List[A])] = {
    require(list.nonEmpty)

    def go(xs: List[A], l: List[A]): List[(List[A], A, List[A])] = {
      l match {
        case Nil() => Nil()
        case y :: ys => (xs, y, ys) :: go(y :: xs, ys)
      }
    }

    go(List(), list)
  } ensuring { _.nonEmpty }

  def interleavings[A](list: List[List[A]]): List[List[A]] = list match {
    case Nil() =>
      List(Nil())

    case xss =>
      zippers(xss) flatMap {
        case (_, Nil(), _) => List(Nil())
        case (xssL, h :: xs, xssR) =>
          val sub = if (xs.isEmpty) Nil[List[A]]() else List(xs)
          interleavings(sub ++ xssL ++ xssR) map { t =>
            h :: t
          }
      }
  }

  val list: List[List[BigInt]] = List(List(1), List(2), List(3))

  val interleaved = force(interleavings(list))

}
