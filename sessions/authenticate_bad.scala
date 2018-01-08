
import stainless.lang._
import stainless.linear._
import stainless.annotation._

import scala.language.postfixOps

import sessions._

object authenticate_bad {

  implicit val timeout: Duration = Duration(500)

  case class Authenticate(card: String, pin: String, cont: LOut[Response])

  sealed abstract class Response // Authentication response from the ATM
  case class Failure()                 extends Response
  case class Success(cont: LOut[Menu]) extends Response

  sealed abstract class Menu // Choices available to authenticated user
  case class CheckBalance(cont: LOut[Balance]) extends Menu
  case class Quit()                                   extends Menu

  case class Balance(amount: BigInt) // User account balance

  def authenticated(card: String, pin: String): Boolean = {
    card == "123456" && pin == "F1"
  }

  def atm(c: LIn[Authenticate]): Unit = {
    c ? { auth => auth! match {
      case Authenticate(_, pin, cont) if authenticated(card, pin) =>
        cont !! (Success(_: LOut[Menu]))

      case Authenticate(_, _, cont) =>
        // cont ! Failure()
    } }
  }

  def menu(menu: Linear[Menu]): Unit = {
    menu! match {
      case CheckBalance(cont) =>
        cont ! Balance(42)
        cont ! Balance(13)

      case Quit() => ()
    }
  }

}

