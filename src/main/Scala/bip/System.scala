package bip

/** Describes a BIP system. */
class System {

  /** The connector of the system. */
  private var _connector: ClosedConnector[_] = ClosedConnector(Failure)

  /** All initial atoms of the system, with their associated behaviour. */
  private var _atoms: Seq[(Atom, () => Unit)] = Seq()

  /** System options. */
  private var _options = SystemOptions(_.head, true)

  /** Creates a new port.
    *
    * @tparam D The type of values that can be received by the port.
    * @tparam U The type of values that can be sent through the port.
    */
  def newPort[D, U](): Port[D, U] = Port.newInstance()

  /** Creates a new atom.
    *
    * @param action The behaviour of the atom.
    */
  def newAtom(action: => Unit): Atom = {
    val atom = Atom.newInstance()
    _atoms = (atom, () => action) +: _atoms
    atom
  }

  /** Registers the connector of the system.
    *
    * @param connector The connector to be used.
    */
  def registerConnector[D, U <: D](connector: Connector[D, U]) {
    _connector = ClosedConnector(connector)
  }

  /** Starts executing the system. */
  def run() {
    if (_atoms.isEmpty) {
      // Nothing to do.
      return
    }
    val engine = new Engine(_connector, _atoms, _options)
    engine.run()
  }

  /** Sets whether or not early execution of stable interactions should
    * be enabled.
    *
    * By default, this option is enabled.
    */
  def setEarlyExecution(value: Boolean) {
    _options = _options.copy(earlyExecution = value)
  }

  /** Sets the function that must be used to pick an interaction.
    *
    * By default, the first possible interaction is selected.
    */
  def setInteractionPicker(picker: Stream[Interaction] => Interaction) {
    _options = _options.copy(interactionPicker = picker)
  }
}

/** Options for the system.
  *
  * @param interactionPicker The function which picks 
  *                          the interaction to execute.
  * @param earlyExecution    Indicates whether or not early execution
  *                          of stable interactions should be enabled.
  */
private case class SystemOptions(
  interactionPicker: Stream[Interaction] => Interaction,
  earlyExecution: Boolean)