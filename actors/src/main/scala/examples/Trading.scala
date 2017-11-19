
package actors

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object trading {

  // case class PrimBehav(counter: Counter) extends Behavior {

  //   override
  //   def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
  //     case Inc() =>
  //       Backup() ! Deliver(counter.increment)
  //       PrimBehav(counter.increment)

  //     case _ => Behavior.same
  //   }
  // }

}
