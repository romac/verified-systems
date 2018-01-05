package sessions

import stainless.lang._
import stainless.util.Random
import stainless.linear._
import stainless.annotation._

import scala.language.postfixOps

object greet_bad {

  implicit val timeout: Duration = Duration(500)

  sealed abstract class Start
  case class Greet(p: String, cont: Linear[Out[Greeting]]) extends Start
  case class Quit() extends Start

  sealed abstract class Greeting
  case class Hello(p: String, cont: Linear[Out[Start]]) extends Greeting
  case class Bye(p: String) extends Greeting

  implicit val state = Random.newState

  @extern
  def continue: Boolean = Random.nextBoolean

  def client(c: Linear[Out[Start]]): Unit = {
    if (continue) {
      val c2 = c !! ((next: Linear[Out[Greeting]]) => Greet("Alice", next))
      c2 ? { greeting =>
        greeting! match {
          case Hello(name, cont) => client(cont)
          case Bye(name) => ()
        }
      }
      // linear term `c2` has already been used
      // linear variable `x` of type `Linear[Greeting]` is never used
      c2 ? { case x => () }
    } else {
      c ! Quit()
      // linear term `c` has already been used
      c ! Quit()
    }
  }

  def server(c: Linear[In[Start]]): Unit = {
    c ? { start =>
      start! match {
        case Greet(whom, cont) => {
          val c2in = cont !! (Hello(whom, _: Linear[Out[Start]]))
          server(c2in)
        }
        case Quit() => ()
      }
    }
  }

}
