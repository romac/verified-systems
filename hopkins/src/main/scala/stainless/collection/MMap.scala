
package stainless.collection

import stainless.lang._

case class MMap[A, B](f: A => Option[B]) {

  def apply(k: A): B = {
    require(contains(k))
    f(k).get
  }

  def get(k: A): Option[B] = {
    f(k)
  } ensuring { res => contains(k) ==> res.isDefined }

  def updated(k: A, v: B): MMap[A, B] = {
    MMap((x: A) => if (x == k) Some(v) else f(x))
  } ensuring { _.contains(k) }

  def getOrElse(k: A, v: B): B = {
    f(k).getOrElse(v)
  } ensuring { res => contains(k) || res == v }

  def contains(k: A): Boolean = {
    f(k).isDefined
  }

}

object MMap {
  def empty[A, B]: MMap[A, B] = MMap((k: A) => None[B]())
}

