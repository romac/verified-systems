
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
            become(Behavior.Unhandled)
          }
        }
      }
    }
  }

  val system = System.initial(MainBehavior, "main")

  @force
  val result = system.run(Nil())

  @extern
  def main(args: Array[String]): Unit = {
    // println(result._2.toScala.mkString("\n"))
  }

}
