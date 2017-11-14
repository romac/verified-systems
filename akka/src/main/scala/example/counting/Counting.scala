
package stainless
package actors

// import stainless.actors._
import stainless.lang._
import stainless.collection._

object Counting {

  case object PrimaryId extends ActorId
  case object BackupId extends ActorId

  object Primary {
    case object Inc extends Message
    case object Ack extends Message
  }

  object Backup {
    case class Inc(replyTo: ActorRef) extends Message
  }

  def primary(backupRef: ActorRef, counter: BigInt): Behavior = Actor.immutable { (ctx, msg, net) =>
    implicit val context: ActorContext = ctx
    implicit val network: Network = net

    msg match {
      case Primary.Inc =>
        backupRef ! Backup.Inc(ctx.self)
        primary(backupRef, counter + 1)

      case _ =>
        Behavior.same
    }
  }

  def backup(counter: BigInt): Behavior = Actor.immutable { (ctx, msg, net) =>
    implicit val context: ActorContext = ctx
    implicit val network: Network = net

    msg match {
      case Backup.Inc(replyTo) =>
        replyTo ! Primary.Ack
        backup(counter + 1)

      case _ =>
        Actor.same
    }
  }

}

object Main {

  import Counting._

  case object MainId extends ActorId

  val main = Actor.deferred { (ctx, net) =>
    implicit val context: ActorContext = ctx
    implicit val network: Network = net

    println("Main actor is running")

    val backupRef = ctx.spawn(backup(0), BackupId)
    val primaryRef = ctx.spawn(primary(backupRef, 0), PrimaryId)

    primaryRef ! Primary.Inc

    Actor.unhandled
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem(main, MainId)
    system.run(Network())
  }

}

