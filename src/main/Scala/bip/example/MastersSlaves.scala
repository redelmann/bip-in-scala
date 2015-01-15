
package bip.example

import bip._
import bip.actions._
import bip.connectors._

/** Dining philosophers example */
object MastersSlaves {

  /** Starts the example. */
  def main(args: Array[String]) {

    // Number of masters.
    val m = 30

    // Number of slaves
    val s = 60

    // Creating the system.
    val system = new System

    // Disables early execution of stable interactions.
    system.setEarlyExecution(false)

    // Ports of slaves. 
    val reserve = system.newPort[Any, Atom]
    val use     = system.newPort[Any, Unit]

    // Creation of the n slaves.
    val slaves: Array[Atom] = for {
      i <- (0 until s).toArray
    } yield system.newAtom {

      // Defines the behaviour of the slave.
      def act() {
        // Sending the identifier of the atom to the port. 
        await(reserve, getSelf()) { (_: Any) =>
          println("Slave " + i + " now reserved")
          // The slave is now reserved.
          await(use) { (_: Any) =>
            // The slave has been used and can now
            // loop back to its initial state.
            act()
          }
        }
      }

      // Starting the behaviour.
      act()
    }

    // Ports of masters.
    val request = system.newPort[Atom, Unit]
    val compute = system.newPort[Any, (Atom, Atom)]

    // Creation of the m masters.
    val masters: Array[Atom] = for {
      i <- (0 until m).toArray
    } yield system.newAtom {
      var j = 1;

      def act() {
        // First, request a first atom.
        await(request) { (first: Atom) =>
          // Then, request a second atom.
          await(request) { (second: Atom) =>
            // Triggers a computations,
            // using the two previously requested atoms.
            await(compute, (first, second)) { (_: Any) =>
              println("Master " + i + " computing for the time " + j)
              j += 1
              // The master loops back to its original state.
              act()
            }
          }
        }
      }

      // Starting the behaviour.
      act()
    }

    // For each master, the following array contains
    // a connector that requests a slave for the connector.
    val reqs = for {
      i <- 0 until m
    } yield {
      // Synchronises the master with any of the slaves.
      close(masters(i).bind(request).andRight(dynamic(reserve)))
    }

    // For each master, the following array contains
    // a connector that uses the two previously reserved slaves.
    val comps = for {
      i <- 0 until m
    } yield {
      // First, obtains the two atoms sent as upwards value by the master.
      masters(i).bind(compute).flatMap { (atoms: (Atom, Atom)) =>
        // Synchronises with the two slaves.
        close(atoms._1.bind(use).and(atoms._2.bind(use)))
      }
    }

    // The connector simply states that an interaction
    // consists of many requests and computations
    system.registerConnector(manyOf(reqs ++ comps : _*))

    // Starts the system.
    system.run()
  }
}