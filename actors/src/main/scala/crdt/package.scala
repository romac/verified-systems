
package object crdt {

  case class CRDT[A](merge: (A, A) => A)

}
