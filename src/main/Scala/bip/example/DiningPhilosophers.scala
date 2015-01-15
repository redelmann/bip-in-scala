
package bip.example

import scala.util.Random

import bip._
import bip.actions._
import bip.connectors._

/** Dining philosophers example */
object DiningPhilosophers {

  /** Starts the example. */
  def main(args: Array[String]) {

    // Number of philosophers.
    val n = 21

    // Creating the system.
    val system = new System

    // Ports of forks. 
    val allocateFork = system.newPort[Any, Unit]
    val releaseFork  = system.newPort[Any, Unit]

    // Creation of the n forks.
    val forks: Array[Atom] = for {
      i <- (0 until n).toArray
    } yield system.newAtom {

      // Defines the behaviour of the fork.
      def act() {
        // The fork is free.
        await(allocateFork) { (_: Any) =>
          // The fork is taken.
          await(releaseFork) { (_: Any) =>
            act()  // The fork restarts its behaviour.
          }
        }
      }

      // Starting the behaviour.
      act()
    }

    // Ports of philosophers.
    val startEating = system.newPort[Any, Unit]
    val endEating   = system.newPort[Any, Unit]

    // Creation of the n philosophers.
    val philosophers: Array[Atom] = for {
      i <- (0 until n).toArray
    } yield system.newAtom {
      var j = 1;

      // Behaviour of the philosopher.
      def act() {

        // The philosopher waits to start eating.
        await(startEating) { (_: Any) =>
          // The philosopher can now eat.
          println("Philosopher " + i + " eating. (" + j + ")")
          j += 1
          // Eating takes some amount of time.
          //Thread.sleep(1000)

          // The philosopher now requests to start sleeping.
          await(endEating) { (_: Any) =>
            println("Philosopher " + i + " sleeping.")

            // The philosopher restarts its behaviour.
            act()
          }
        }
      }

      // Starting the behaviour.
      act()
    }

    // Contains a connector that requests both forks and
    // makes the philosopher start eating for each philosopher.
    val eatings = for {
      i <- 0 until n
    } yield allOf(
      forks(i) bind allocateFork,
      forks((i + 1) % n) bind allocateFork,
      philosophers(i) bind startEating)

    // Contains a connector that release both forks and
    // makes the philosopher start sleeping for each philosopher.
    val sleepings = for {
      i <- 0 until n
    } yield allOf(
      forks(i) bind releaseFork,
      forks((i + 1) % n) bind releaseFork,
      philosophers(i) bind endEating)

    // Port on which the shuffled list of connectors is sent.
    val shuffled = system.newPort[Any, Seq[Connector[Any, Any]]]

    // Atom that continuously shuffle the list of connectors.
    val shuffler = system.newAtom {

      // Behaviour of the shuffler
      def act() {
        // Shuffling the connectors.
        val connectors = Random.shuffle(eatings)
        await(shuffled, connectors) { (_: Any) =>
          act()  // Looping.
        }
      }

      // Starting the behaviour.
      act()
    }

    // Registering the connector.
    // It first requests a shuffled list of connectors from the
    // shuffler and then synchronizes many of them.
    system.registerConnector(shuffler bind shuffled flatMap {
      case cs => manyOf(cs ++ sleepings : _*)
    })

    // Starts the system.
    system.run()
  }
}