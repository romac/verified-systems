
package stainless.lang

object utils {

  @inline
  def exists[A](f: A => Boolean): Boolean = {
    !forall((x: A) => !f(x))
  }

  @inline
  def exists[A, B](f: (A, B) => Boolean): Boolean = {
    !forall((x: A, y: B) => !f(x, y))
  }

  // @inline
  // def isRefl[A](r: (A, A) => Boolean): Boolean = forall { (x: A) =>
  //   r(x, x)
  // }

  // @inline
  // def isTrans[A](r: (A, A) => Boolean): Boolean = forall { (x: A, y: A, z: A) =>
  //   (r(x, y) && r(y, z)) ==> r(x, y)
  // }

  // @inline
  // def isReflTrans[A](r: (A, A) => Boolean): Boolean = {
  //   isRefl(r) && isTrans(r)
  // }

}
