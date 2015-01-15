package bip

/** Represents a port of a BIP system.
  *
  * @tparam D The type of values that can be received by the port.
  * @tparam U The type of values that can be sent through the port.
  * @param identifier The identifier of the atom.
  */
case class Port[D, U] private (identifier: Int) {

  /** Returns an object that specifies that the given value
    * should be sent on this port.
    */
  def withValue(value: U): AwaitCase[D, U, D] =
    AwaitCase(this, value, (x: D) => x)
}

/** Companion object of [[bip.Port]]. */
private object Port {
  var _nextIdentifier = 0;

  /** Creates a new unique port instance. */
  def newInstance[D, U](): Port[D, U] = {
    val identifier = synchronized {
      val i = _nextIdentifier
      _nextIdentifier += 1
      i
    }
    Port(identifier)
  }
}