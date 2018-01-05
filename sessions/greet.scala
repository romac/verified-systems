package sessions

import stainless.lang._
import stainless.io.StdOut._
import stainless.io.StdIn._
import stainless.linear._
import stainless.annotation._

import scala.language.postfixOps

object greet {

  implicit val timeout: Duration = Duration(60 * 1000)

  sealed abstract class Start
  case class Greet(p: String, cont: Linear[Out[Greeting]]) extends Start
  case class Quit() extends Start

  sealed abstract class Greeting
  case class Hello(p: String, cont: Linear[Out[Start]]) extends Greeting
  case class Bye(p: String) extends Greeting

  implicit val ioState = stainless.io.newState

  def continue(): Boolean = {
    val action = readLine("What do you want to do? [quit/greet] ")
    action match {
      case "quit"  => false
      case "greet" => true
      case _       => continue()
    }
  }

  def client(c: Linear[Out[Start]]): Unit = {
    if (continue()) {
      val name = readLine("Who do you want to greet? ")

      val c2 = c !! (Greet(name, _: Linear[Out[Greeting]]))

      c2 ? { greeting =>
        greeting! match {
          case Hello(name, cont) =>
            println("Server: Server says 'Hello " + name + "'")
            client(cont)

          case Bye(name) => 
            println("Server: Server says 'Bye " + name + "'")
        }
      }
    } else {
      c ! Quit()
    }
  }

  def server(c: Linear[In[Start]]): Unit = {
    c ? { start =>
      start! match {
        case Greet(whom, cont) => {
          println("Server: Got greeted by " + whom)
          val c2in = cont !! (Hello(whom, _: Linear[Out[Start]]))
          server(c2in)
        }
        case Quit() =>
          println("Server: Client wants to quit")
      }
    }
  }

//   @ignore
//   def main(args: Array[String]): Unit = {
//     import java.util.concurrent.CountDownLatch

//     def await(tasks: List[() => Unit]): Unit = {
//       val latch = new CountDownLatch(tasks.length)

//       tasks foreach { task =>
//         fork(task, latch).start()
//       }
//     }

//     def fork(compute: () => Unit, latch: CountDownLatch): Thread = {
//       new Thread(new Runnable {
//         def run(): Unit = {
//           compute()
//           latch.countDown()
//         }
//       })
//     }

//     val (cin, cout) = LocalChannel.factory[Start]()
//     await(List(
//       () => client(cout),
//       () => server(cin)
//     ))
//   }

}
