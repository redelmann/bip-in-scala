
package bip

/** Open interaction of a system.
  *
  * @tparam D The type of the value needed in the downwards phase.
  * @tparam U The type of the value provided in the upwards phase.
  * @param upwards   The value transmitted during the upwards phase.
  * @param downwards The function that computes the action to be executed by
  *                  each of the involved atoms.
  * @param involved  The atoms involved in the interaction.
  */
private case class OpenInteraction[-D, +U](
    upwards: U, 
    downwards: D => Seq[(Atom, () => Unit)], 
    involved: Set[Atom])


/** Closed interaction of a system.
  *
  * @param tasks A list of actions to be performed by atoms.
  */
private case class Interaction(tasks: Seq[(Atom, () => Unit)])


/** Companion  object of [[bip.Interaction]]. */
private object Interaction {

  /** Closes an open interaction and produces a closed interaction. */
  def close[A](open: OpenInteraction[A, A]): Interaction = open match {
    case OpenInteraction(upwards, downwards, _) =>
      Interaction(downwards(upwards))
  }
}