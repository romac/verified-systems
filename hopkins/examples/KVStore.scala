
package hopkins

import stainless.lang._
import stainless.proof._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

object initialState {
  import replicated._

  def apply(node: Node): State = node match {
    case Client() => Value()
    case Server() => KV()
  }
}

object replicated {

  ////////////////
  // DEFINITIONS
  ////////////////

  case class Server() extends Node {

    def processMsg(msg: Msg, state: State): Result = (msg, state) match {
      case (Get(k), kv @ KV(_)) =>
        val msg = kv.get(k) match {
          case Some(v) => Found(k, v)
          case None() => NotFound(k)
        }
        Result(kv, List(Packet(Client(), msg)), Nil())

      case (Put(k, v), kv @ KV(_)) =>
        Result(kv.put(k, v), List(Packet(Client(), Ack(k, v))), Nil())

      case _ =>
        Result(state, Nil(), Nil())
    }

    def processInput(input: Input, state: State): Result =
      Result(state, Nil(), Nil())

  }

  case class Client() extends Node {

    def processMsg(msg: Msg, state: State): Result = (msg, state) match {
      case (NotFound(k), Value(v)) => Result(Value(None()), Nil(), List(Done(v)))
      case (Found(k, v), Value(v)) => Result(Value(Some(v)), Nil(), List(Done(v)))
      case _                       => Result(state, Nil(), Nil())
    }

    def processInput(input: Input, state: State): Result = (input, state) match {
      case (Start(), v: Value) =>
        val msg = List(Packet(Server(), Put("hello", "world")))
        Result(v, msg, Nil())

      case _ =>
        Result(state, Nil(), Nil())
    }

  }

  case class Get(key: String) extends Msg
  case class Found(key: String, value: String) extends Msg
  case class NotFound(key: String) extends Msg
  case class Put(key: String, value: String) extends Msg
  case class Ack(key: String, value: String) extends Msg

  case class Value(value: Option[String] = None()) extends State

  case class KV(store: MMap[String, String] = MMap.empty) extends State {
    def get(k: String): Option[String] = store.get(k)
    def put(k: String, v: String): KV = KV(store.updated(k, v))
  }

  case class Start() extends Input
  case class Done(result: Option[String]) extends Output

  ////////////////
  // THEOREMS
  ////////////////

}
