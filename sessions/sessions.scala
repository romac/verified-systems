import stainless.lang._
import stainless.linear._
import stainless.collection._
import stainless.annotation._

package object sessions {

  case class Duration(millis: Long)

  @linear
  @library
  class In[A] {

    @extern
    def receive(implicit d: Duration): Linear[A] = {
      ???
    }

    def ?[B](f: Linear[A] => B)(implicit d: Duration): B = {
      f(receive)
    }

  }

  @linear
  @library
  class Out[A] {

    @extern
    def send(msg: A): Unit = {
      ???
    }

    def !(msg: A): Unit = {
      send(msg)
    }

    @extern @unchecked
    def !![B](h: Linear[Out[B]] => A): Linear[In[B]] = {
      val (cin, cout) = create[B]()
      this ! h(cout)
      cin
    }

    // @extern @unchecked
    // def !![B](h: Linear[In[B]] => A): Linear[Out[B]] = {
    //   val (cin, cout) = create[B]()
    //   this ! h(cin)
    //   cout
    // }

    @extern @unchecked
    def create[B](): (Linear[In[B]], Linear[Out[B]]) = {
      ???
    }
  }

  // @library
  // case class AbstractIn[A](a: Linear[A]) extends In[A] {
  //   override def ?[B](f: Linear[A] => B): B = {
  //     f(a)
  //   }
  // }

  // @library
  // case class AbstractOut[A]() extends Out[A] {
  //   override def !(msg: A): Unit = {
  //     ()
  //   }
  // }

}
