package bip

import scala.util.control.ControlThrowable

/** Executes a BIP system.
  *
  * @param closed       The connector of the system.
  * @param initialTasks The initial tasks to execute
  *                     at the beginning of the system.
  * @param options      User specified options.
  */
private class Engine(
    closed: ClosedConnector[_],
    initialTasks: Seq[(Atom, () => Unit)],
    val options: SystemOptions) {
  
  /** Waiting list of the system. */
  private val _waitingList = new WaitingList

  /** Number of currently running atoms. */
  private var _running = 0

  /** Indicates whether a worker thread has signaled the main loop. */
  private var _signaled = false

  /** Executes the engine. */
  def run() {

    // Tasks to execute.
    var tasks = initialTasks

    // Main loop of the engine.
    while (true) {

      // If there are some tasks to execute,
      // start the worker threads.
      if (!tasks.isEmpty) {
        modifyRunningCount(tasks.length)

        tasks.foreach {
          case (atom, action) => {
            spawnAtom(atom, action)
          }
        }
      }
      tasks = Seq()

      // Wait for the main loop to be signaled.
      synchronized {
        if (!_signaled) {
          wait()
        }
        assert(_signaled)
        _signaled = false
      }

      // Checks if the system is in a stable state.
      val stable = synchronized {
        _running == 0
      }

      // Computes the next interaction.
      _waitingList.synchronized {
        val interactionsStream =
          if (stable) closed.getInteractions(_waitingList)
          else closed.getStableInteractions(_waitingList)

        val nonEmptyInteractionsStream =
          interactionsStream.filter(!_.tasks.isEmpty)

        // Checks if an interaction is possible.
        if (!nonEmptyInteractionsStream.isEmpty) {
          
          // Picks an interaction.
          tasks = options.interactionPicker(nonEmptyInteractionsStream).tasks

          // Removes the involved atoms from the waiting list.
          tasks.foreach {
            case (atom, _) => _waitingList.setRunning(atom)
          }

          // Signal ourselve if early execution is enabled.
          if (options.earlyExecution) {
            _signaled = true
          }
        }
        else if (stable) {
          // Not a single possible interaction.
          return
        }
      }
    }
  }

  /** Modifies the number of running atoms. */
  def modifyRunningCount(delta: Int): Int = synchronized {
    _running += delta
    _running
  }

  /** Signals the main loop that it main continue its execution. */
  def signal() {
    synchronized {
      _signaled = true
      notify()
    }
  }

  /** Creates and starts a worker thread.
    *
    * @param atom   The atom whose action is executed.
    * @param action The action to be performed by the atom.
    */
  def spawnAtom(atom: Atom, action: () => Unit) {
    val thread = new AtomExecutorThread(this, atom, action)
    thread.start()
  }

  /** Registers in the waiting list of the engine that the
    * atom is waiting on the given ports.
    *
    * @param atom  The atom that is declared as waiting.
    * @param cases The different ports and associated values.
    * @param cont  The continuation of the atom.
    */
  def setWaiting[A](
      atom: Atom,
      cases: Seq[AwaitCase[_, _, A]], 
      cont: A => Unit) {

    _waitingList.synchronized {
      cases.foreach {
        case AwaitCase(port, value, function) => 
          _waitingList.setWaiting(atom, port, value, function andThen cont)
      }
    }
  }
}


/** Worker thread of the engine.
  *
  * @param engine The engine which created the thread.
  * @param atom   The atom whose behaviour is being executed.
  * @param action The action that must performed by the atom.
  */
private class AtomExecutorThread(
    engine: Engine,
    val atom: Atom,
    action: () => Unit) extends Thread {
  
  /** Runs the action and handles all calls to atom instructions. */
  override def run() {
    try {
      // Try executing the action.
      action()
    }
    catch {
      // Handling calls to await and awaitAny.
      case AwaitException(cases, cont) =>
        engine.setWaiting(atom, cases, cont)
    } 
    finally {
      // Modifying the number of currently executing atoms.
      val running = engine.modifyRunningCount(-1)

      // Signaling the main loop if necessary.
      if (running == 0 || engine.options.earlyExecution) {
        engine.signal()
      }
    }
  }

  /** Creates a new atom and returns its identifier. */
  def spawnAtom(action: () => Unit): Atom = {
    engine.modifyRunningCount(1)
    val newAtom = Atom.newInstance()
    engine.spawnAtom(newAtom, action)
    newAtom
  }
}

/** Control exception thrown by calls to [[bip.actions.await]]
  * or [[bip.actions.awaitAny]].
  *
  * Will be caught by the [[bip.AtomExecutorThread]] executing the atom.
  *
  * @param cases The different values sent on the different ports.
  * @param cont  The continuation of the atom.
  */ 
private case class AwaitException[A](
    cases: Seq[AwaitCase[_, _, A]],
    cont: A => Unit) extends Exception with ControlThrowable
