package bip

import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashSet

/** Contains all information about waiting atoms. */
private class WaitingList {

  /** Main mapping, contains all entries */
  val portsForAtom: HashMap[Atom, HashMap[Port[_, _], WaitCase[_, _]]] = 
    HashMap.empty

  /** Secondary mapping, contains all atoms waiting on a specific port. */
  val atomsForPort: HashMap[Port[_, _], LinkedHashSet[Atom]] = HashMap.empty


  /** Records that the atom is waiting on the specified port.
    *
    * @param atom The waiting atom.
    * @param port The activated port.
    * @param sent The value sent by the atom through the port.
    * @param cont The continuation of the atom.
    */
  def setWaiting[D, U](
      atom: Atom,
      port: Port[D, U],
      sent: U,
      cont: D => Unit) {

    val set = atomsForPort.getOrElseUpdate(port, LinkedHashSet.empty)
    set += atom

    val map = portsForAtom.getOrElseUpdate(atom, HashMap.empty)
    map(port) = WaitCase(sent, cont)
  }

  /** Specifies that the atom is no longer waiting.
    *
    * Removes all entries related to the atom in the data structure.
    *
    * @param atom The atom that is no longer waiting.
    */
  def setRunning(atom: Atom) {

    val binding = portsForAtom.remove(atom)

    binding match {
      case Some(ports) => ports.keys.foreach {
        case port => {
          val set = atomsForPort(port)
          set -= atom
          if (set.isEmpty) {
            atomsForPort -= port
          }
        }
      }
      case None => ()  // Atoms was already running.
    }
  }


  /** Returns the entry related to the atom and port. */
  def getBoundInteraction[D, U](atom: Atom, port: Port[D, U]):
      Option[OpenInteraction[D, U]] = for {
    ports <- portsForAtom.get(atom)
    untypedWaitCase <- ports.get(port)
  } yield {
    val waitCase: WaitCase[D, U] = untypedWaitCase.asInstanceOf[WaitCase[D, U]]

    OpenInteraction(
      waitCase.upwards, 
      (x: D) => Seq((atom, () => waitCase.downwards(x))), 
      Set(atom))
  }

  /** Returns all entries related to the port. */
  def getBoundInteractions[D, U](port: Port[D, U]):
      Stream[OpenInteraction[D, U]] = {
    val atoms = atomsForPort.getOrElse(port, LinkedHashSet.empty)

    val zero: Seq[OpenInteraction[D, U]] = Seq()

    atoms.foldRight(zero)({
      case (atom, rest) => getBoundInteraction(atom, port) match {
        case None => rest
        case Some(interaction) => interaction +: rest
      }
    }).toStream
  }
}

/** Entry of the [[bip.WaitingList]]. */
private case class WaitCase[D, U](upwards: U, downwards: D => Unit)