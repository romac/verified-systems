


def zippers[A](list: List[A]): List[(List[A], A, List[A])] = {
  def go(xs: List[A], l: List[A]): List[(List[A], A, List[A])] = l match {
    case Nil => Nil
    case y :: ys => (xs, y, ys) :: go(y :: xs, ys)
  }

  go(List(), list)
}

/*
interleavings :: [[a]] -> [[a]]
interleavings [] = [[]]
interleavings xss = do
    (xssL, h:xs, xssR) <- zippers xss
    t <- interleavings ([xs | not (null xs)] ++ xssL ++ xssR)
    return (h:t)
*/

def interleavings[A](xss: List[List[A]]): List[List[A]] = {
  val zs = zippers(xss)
  println(zs)
  
  zs flatMap {
    case (xssL, h :: xs, xssR) =>
      interleavings((if (xs.isEmpty) Nil else List(xs)) ++ xssL ++ xssR) map { t =>
        println(t)
        h :: t
      }
    case _ => Nil
  }
}

//zippers(List(1, 2, 3))
println(interleavings(List(List(1, 2, 3))))
