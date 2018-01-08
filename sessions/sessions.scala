import stainless.lang._
import stainless.linear._
import stainless.collection._
import stainless.annotation._

package object sessions {

  case class Duration(millis: Long)

  type In[A]  = Linear[InChan[A]]
  type Out[A] = Linear[OutChan[A]]

  @linear
  @library
  class InChan[A] {

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
  class OutChan[A] {

    @extern
    def send(msg: A): Unit = {
      ???
    }

    def !(msg: A): Unit = {
      send(msg)
    }

    @extern @unchecked
    def !![B](h: Out[B] => A): In[B] = {
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
    def create[B](): (In[B], Out[B]) = {
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
