
package hopkins

import stainless.annotation._

object initialBehavior {
  @extern
  def forActor(actor: ActorId): Behavior = Behavior.Stopped()
}

