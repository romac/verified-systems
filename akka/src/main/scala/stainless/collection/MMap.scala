
package stainless.collection

import stainless.lang._

case class MMap[A, B](f: A => Option[B]) {

  def contains(k: A): Boolean = {
    f(k) != None[B]()
    // f(k) != None
  }

  def apply(k: A): B = {
    require(contains(k))
    f(k).get
  }

  def updated(k: A, v: B): MMap[A, B] = {
    MMap((x: A) => if (x == k) Some(v) else f(x))
  } ensuring(_.contains(k))

  def getOrElse(k: A, v: B): B = {
    if (contains(k)) f(k).get else v
  }
}

object MMap {
  def empty[A, B](): MMap[A, B] = MMap((x: A) => None[B]())
  // def empty[A, B](): MMap[A, B] = MMap((x: A) => None)
}

