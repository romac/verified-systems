
package actario

import stainless.lang._
import stainless.annotation._
import stainless.collection._

object counter {

  object Primary {
    final case object Inc extends Msg
    final case object Ack extends Msg
  }

  object Backup {
    final case class Inc(replyTo: Name) extends Msg
  }

  case class Counter(value: BigInt) {
    require(value >= 0)

    @inline
    def increment: Counter = Counter(value + 1)
  }

  case class PrimaryBehavior(backupRef: Name, counter: Counter) extends Behavior {
    import Action._

    def receive(msg: Msg): Action = msg match {
      case Primary.Inc =>
        self { primaryRef =>
          send(backupRef, Backup.Inc(primaryRef)) {
            become(PrimaryBehavior(backupRef, counter.increment))
          }
        }

      case Primary.Ack =>
        become(same)

      case _ =>
        become(same)
    }
  }

  case class BackupBehavior(counter: Counter) extends Behavior {
    import Action._

    def receive(msg: Msg): Action = msg match {
      case Backup.Inc(replyTo) =>
        send(replyTo, Primary.Ack) {
          become(BackupBehavior(counter.increment))
        }

      case _ =>
        become(same)
    }
  }

  case object MainBehavior extends Behavior {
    import Action._

    def receive(msg: Msg): Action = {
      spawn(BackupBehavior(Counter(0))) { backup =>
        spawn(PrimaryBehavior(backup, Counter(0))) { primary =>
          send(primary, Primary.Inc) {
          send(primary, Primary.Inc) {
          send(primary, Primary.Inc) {
            become(Behavior.Unhandled)
          } } }
        }
      }
    }
  }

  val system = System.initial(MainBehavior, "main")

  @force
  val result = system.run(Nil())

  def inv(bp: (BigInt, BigInt)): Boolean = bp._1 <= bp._2

  @force
  def invariant = {
    val main = Name.Toplevel("main")
    val backup = Name.Generated(1, main)
    val primary = Name.Generated(2, main)

    val init = (BigInt(0), BigInt(0))
    val trace = result._2

    System.invHolds(trace, init, inv) { case ((b, p), label) =>
      label match {
        case Label.Become(name, behav) if name == primary =>
          val PrimaryBehavior(_, pc) = behav
          (b, pc.value)

        case Label.Become(name, behav) if name == backup =>
          val BackupBehavior(bc) = behav
          (bc.value, p)

        case _ => (b, p)
      }
    }
  }

  @extern
  def main(args: Array[String]): Unit = {
    // println(result)
    println(invariant)
  }

}
