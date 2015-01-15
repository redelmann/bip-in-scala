package bip

/** Represents an atom of a BIP system.
  *
  * @param identifier The identifier of the atom.
  */
case class Atom private (identifier: Int) {

  /** Returns a connector that binds a port to this atom. */
  def bind[D, U](port: Port[D, U]): Connector[D, U] = Bind(this, port)
}

/** Companion object of [[bip.Atom]]. */
private object Atom {
  var _nextIdentifier = 0;

  /** Creates a new unique atom instance. */
  def newInstance(): Atom = {
    val identifier = synchronized {
      val i = _nextIdentifier
      _nextIdentifier += 1
      i
    }
    Atom(identifier)
  }
}

/** Contains a port and a value to be sent on that port.
  * Also contains a function to apply on the received value.
  *
  * @tparam D The type of values that can be received by the port.
  * @tparam U The type of values that can be sent by the port.
  * @tparam A The type of the received value, after being transformed.
  * @param port     The port on which to send a value.
  * @param value    The value to send.
  * @param function The function to apply on the received value.
  * @see See the method [[Port.withValue]] to create values of this type.
  */
case class AwaitCase[D, U, A] private[bip] (
    port: Port[D, U],
    value: U,
    function: D => A) {

  /** Chains another function to be applied on the received value. */
  def map[B](other: A => B): AwaitCase[D, U, B] =
    AwaitCase(port, value, function andThen other)
}

/** This package contains actions that can be performed by atoms.
  *
  * All of the functions defined below are meant to be used by atoms only.
  */
package object actions {

  /** Sends a value on potentially multiple ports, and waits for
    * any of them to receive a value back in return.
    *
    * @tparam A The type of values that can be received.
    * @param cases The different ports and values to send.
    * @param cont  What to do with the value received.
    *
    * @note This function never returns normally.
    */
  def awaitAny[A](cases: AwaitCase[_, _, A]*)(cont: A => Unit): Nothing = {
    // We throw a control exception containing all information we need.
    throw new AwaitException(cases, cont)
  }


  /** Sends a value on a port, and waits to receive a value back in return.
    *
    * @tparam D The type of values that can be received.
    * @tparam U The type of values that can be sent.
    * @param port  The port on which to wait.
    * @param value The value to be sent.
    * @param cont  What to do with the value received.
    *
    * @note This function never returns normally.
    */
  def await[D, U](port: Port[D, U], value: U)(cont: D => Unit): Nothing = 
    awaitAny(AwaitCase(port, value, (x: D) => x))(cont)


  /** Sends a useless value on a port, 
    * and waits to receive a value back in return.
    *
    * @tparam D The type of values that can be received.
    * @tparam U The type of values that can be sent.
    * @param port  The port on which to wait.
    * @param value The value to be sent.
    * @param cont  What to do with the value received.
    *
    * @note This function never returns normally.
    */
  def await[D](port: Port[D, Unit])(cont: D => Unit): Nothing =
    await(port, ())(cont)


  /** Spawns a new atom.
    *
    * @param action The action to execute.
    * @return The newly created atom.
    */
  def spawnAtom(action: => Unit): Atom = {
    // Gets the current thread.
    val thread: Thread = Thread.currentThread()

    // Ensures that the thread has been created by the engine.
    if (!thread.isInstanceOf[AtomExecutorThread]) {
      throw new Error("spawnAtom wasn't called from an atom.")
    }

    // Converts to the specific type of threads.
    val atomThread = thread.asInstanceOf[AtomExecutorThread]

    // Calls the method on the thread.
    atomThread.spawnAtom(() => action)
  }


  /** Returns the currently executing atom. */
  def getSelf(): Atom = {
    // Gets the current thread.
    val thread: Thread = Thread.currentThread()

    // Ensures that the thread has been created by the engine.
    if (!thread.isInstanceOf[AtomExecutorThread]) {
      throw new Error("getSelf wasn't called from an atom.")
    }

    // Converts to the specific type of threads.
    val atomThread = thread.asInstanceOf[AtomExecutorThread]

    // Gets the atom being executed by the thread.
    atomThread.atom
  } 
}