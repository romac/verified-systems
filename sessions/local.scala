
package sessions

import stainless.lang._
import stainless.linear._

import scala.concurrent.{Future, Promise, Await}
import scala.concurrent.duration

class LocalIn[A](val future: Future[Linear[A]]) extends In[A] {

  override def receive(implicit d: Duration): Linear[A] = {
    val timeout = duration.Duration(d.millis, "millis")
    Await.result(future, timeout)
  }
}

class LocalOut[A](val promise: Promise[Linear[A]]) extends Out[A] {

  override def send(msg: A): Unit = {
    promise.success(msg)
  }

  override def create[B](): (Linear[LocalIn[B]], Linear[LocalOut[B]]) = {
    LocalChannel.factory()
  }
}

object LocalChannel {
  def factory[A](): (Linear[LocalIn[A]], Linear[LocalOut[A]]) = {
    val promise = Promise[Linear[A]]()
    val future  = promise.future
    (new LocalIn(future), new LocalOut(promise))
  }
}

