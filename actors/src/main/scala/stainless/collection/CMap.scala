
package stainless.collection

import stainless.lang._

case class CMap[A, B](f: A => B) {

  def apply(k: A): B = {
    f(k)
  }

  def updated(k: A, v: B): CMap[A, B] = {
    CMap((x: A) => if (x == k) v else f(x))
  }

  def getOrElse(k: A, v: B): B = {
    f(k)
  }

  def contains(k: A): Boolean =
    true

}

