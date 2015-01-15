package bip

import scala.math.PartialOrdering

/** Connects together the ports of a system.
  *
  * @tparam D The type of values accepted during the downwards phase.
  * @tparam U The type of values propagated during the upwards phase.
  */
sealed abstract class Connector[-D, +U] {

  /** Applies a function on the upwards value. */
  def map[V](function: U => V): Connector[D, V] = Mapped(function, this)

  /** Applies a function on the downwards value. */
  def contramap[E](function: E => D):
      Connector[E, U] = ContraMapped(function, this)

  /** Applies a function on the upwards value and synchronize
    * with the connector produced by the function.
    */
  def flatMap[V >: U, E <: D](function: U => Connector[E, V]):
      Connector[E, V] = Joined(Mapped(function, this))

  /** Ensures that a predicate holds on the upwards value. */
  def filter(predicate: U => Boolean):
      Connector[D, U] = Guarded(predicate, this)

  /** Ensures that a predicate holds on the upwards value. */
  def withFilter(predicate: U => Boolean):
      Connector[D, U] = Guarded(predicate, this)

  /** Specifies an alternative to this connector. */
  def or[E <: D, V >: U](that: Connector[E, V]): Connector[E, V] =
    OneOf(this, that)

  /** Synchronization of two connectors. Propagates upwards both values. */
  def and[E <: D, V](that: Connector[E, V]): Connector[E, (U, V)] =
    BothOf(this, that)

  /** Synchronization of two connectors. Propagates upwards the left value. */
  def andLeft[E <: D](that: Connector[E, Any]): Connector[E, U] = {
    def first(t: (U, Any)) = t._1
    Mapped(first, BothOf(this, that))
  }

  /** Synchronization of two connectors. Propagates upwards the right value. */
  def andRight[E <: D, V](that: Connector[E, V]): Connector[E, V] = {
    def second(t: (U, V)) = t._2
    Mapped(second, BothOf(this, that))
  }

  /** Replaces the upwards value. */
  def sending[V](value: V): Connector[D, V] =
    Mapped((x: U) => value, this)

  /** Replaces the downwards value. */
  def receiving(value: D): Connector[Any, U] =
    ContraMapped((x: Any) => value, this)

  /** Provides the upwards value to the downwards function. */
  def feedback[E](implicit function: (E, U) => D): Connector[E, U] = {
    val uncurried = (t: (E, U)) => function(t._1, t._2)
    val connector: Connector[(E, U), U] = this.contramap(uncurried)
    Feedback(connector)
  }

  /** Uses the upwards value as the downwards value.
    * Ignores all downwards values it receives.
    * Propagates upwards only the unit value.
    */
  def close(implicit function: U => D): Connector[Any, Unit] = {
    val second = (t: (Any, U)) => t._2
    Feedback(this.contramap(second andThen function)).sending(())
  }

  /** Returns a stream of all currently possible interactions.
    *
    * @param waitingList The data structure holding information
    *                    about all waiting atoms.
    */
  private[bip] def getInteractions(waitingList: WaitingList):
      Stream[OpenInteraction[D, U]] = {

    this.getInteractionsWithout(waitingList, Set())
  }

  /** Returns a stream of all currently possible interactions.
    *
    * @param waitingList The data structure holding information
    *                    about all waiting atoms.
    * @param prohibited  The set of prohibited atoms.
    */
  private[bip] def getInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): Stream[OpenInteraction[D, U]]


  /** Returns a stream of all currently possible and stable interactions.
    *
    * @param waitingList The data structure holding information
    *                    about all waiting atoms.
    */
  private[bip] def getStableInteractions(waitingList: WaitingList):
      Stream[OpenInteraction[D, U]] = {

    this.getStableInteractionsWithout(waitingList, Set())._1
  }

  /** Returns a stream of all currently possible interactions.
    *
    * @param waitingList The data structure holding information
    *                    about all waiting atoms.
    * @param prohibited  The set of prohibited atoms.
    * @return A stream of all possible interactions and
    *         a boolean indicating whether the stream is maximal.
    */
  private[bip] def getStableInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): (Stream[OpenInteraction[D, U]], Boolean) = null
}

// Core combinators

/** Binds a port to an atom. */
private case class Bind[D, U](
    atom: Atom, 
    port: Port[D, U]) extends Connector[D, U] {

  override def getInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): Stream[OpenInteraction[D, U]] = {

    if (prohibited contains atom) {
      Stream()
    }
    else {
      waitingList.getBoundInteraction(atom, port).toStream
    }
  }

  override def getStableInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): (Stream[OpenInteraction[D, U]], Boolean) = {

    val stream = waitingList.getBoundInteraction(atom, port).toStream
    val maximal = !stream.isEmpty

    if (prohibited contains atom) {
      (Stream(), maximal)
    }
    else {
      (stream, maximal)
    }
  }
}

/** Successful connector always propagating the same upwards value. */
private case class Success[U](value: U) extends Connector[Any, U] {

  override def getInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): Stream[OpenInteraction[Any, U]] = {
    
    Stream(OpenInteraction(value, (x: Any) => Seq(), Set()))
  }

  override def getStableInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): (Stream[OpenInteraction[Any, U]], Boolean) = {
    
    (Stream(OpenInteraction(value, (x: Any) => Seq(), Set())), true)
  }
}

/** Failing connector, never enabled. */
private case object Failure extends Connector[Any, Nothing] {
  
  override def getInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): Stream[OpenInteraction[Any, Nothing]] = {

    Stream()
  }

  override def getStableInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): (Stream[OpenInteraction[Any, Nothing]], Boolean) = {

    (Stream(), true)
  }
}

/** Synchronization of two connector. */
private case class BothOf[D, U, V](
    left: Connector[D, U], 
    right: Connector[D, V]) extends Connector[D, (U, V)] {

  override def getInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): Stream[OpenInteraction[D, (U, V)]] = for {

    OpenInteraction(u1, d1, is1) <-
      left.getInteractionsWithout(waitingList, prohibited)
    OpenInteraction(u2, d2, is2) <- 
      right.getInteractionsWithout(waitingList, prohibited union is1)
  } yield OpenInteraction((u1, u2), (x: D) => d1(x) ++ d2(x), is1 union is2)


  override def getStableInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): (Stream[OpenInteraction[D, (U, V)]], Boolean) = {
    var maximal = true

    def removeMaximalTag[A](t: (A, Boolean)): A = {
      maximal &= t._2
      t._1
    }

    val stream = for {
      OpenInteraction(u1, d1, is1) <-
        removeMaximalTag(left.getStableInteractionsWithout(
          waitingList, prohibited))
      OpenInteraction(u2, d2, is2) <- 
        removeMaximalTag(right.getStableInteractionsWithout(
          waitingList, prohibited union is1))
    } yield OpenInteraction((u1, u2), (x: D) => d1(x) ++ d2(x), is1 union is2)

    (stream, maximal)
  }
}

/** Union of two connectors. */
private case class OneOf[D, U](
    left: Connector[D, U],
    right: Connector[D, U]) extends Connector[D, U] {

  override def getInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): Stream[OpenInteraction[D, U]] = {

    left.getInteractionsWithout(waitingList, prohibited) ++
    right.getInteractionsWithout(waitingList, prohibited)
  }

  override def getStableInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): (Stream[OpenInteraction[D, U]], Boolean) = {

    val (lefts, maxLeft) =
      left.getStableInteractionsWithout(waitingList, prohibited)
    val (rights, maxRight) = 
      right.getStableInteractionsWithout(waitingList, prohibited)

    (lefts ++ rights, maxLeft && maxRight)
  }
}


// Data combinators


/** Applies a function on the upwards value. */
private case class Mapped[D, U, V](
    function: U => V,
    connector: Connector[D, U]) extends Connector[D, V] {

  override def getInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): Stream[OpenInteraction[D, V]] = {

    connector.getInteractionsWithout(waitingList, prohibited) map {
      case OpenInteraction(upwards, downwards, involved) =>
        OpenInteraction(function(upwards), downwards, involved)
    }
  }

  override def getStableInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): (Stream[OpenInteraction[D, V]], Boolean) = {

    val (stream, maximal) =
      connector.getStableInteractionsWithout(waitingList, prohibited)

    val mappedStream = stream map {
      case OpenInteraction(upwards, downwards, involved) =>
        OpenInteraction(function(upwards), downwards, involved)
    }

    (mappedStream, maximal)
  }
}

/** Applies a function on the downwards value. */
private case class ContraMapped[D, E, U](
    function: E => D, 
    connector: Connector[D, U]) extends Connector[E, U] {

  override def getInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): Stream[OpenInteraction[E, U]] = {

    connector.getInteractionsWithout(waitingList, prohibited) map {
      case OpenInteraction(upwards, downwards, involved) =>
        OpenInteraction(upwards, function andThen downwards, involved)
    }
  }

  override def getStableInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): (Stream[OpenInteraction[E, U]], Boolean) = {

    val (stream, maximal) =
      connector.getStableInteractionsWithout(waitingList, prohibited)

    val mappedStream = stream map {
      case OpenInteraction(upwards, downwards, involved) =>
        OpenInteraction(upwards, function andThen downwards, involved)
    }

    (mappedStream, maximal)
  }
}

/** Ensures that a predicate holds on the upwards value. */
private case class Guarded[D, U](
    predicate: U => Boolean,
    connector: Connector[D, U]) extends Connector[D, U] {

  override def getInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): Stream[OpenInteraction[D, U]] = {

    connector.getInteractionsWithout(waitingList, prohibited) filter {
      case OpenInteraction(upwards, _, _) => predicate(upwards)
    }
  }

  override def getStableInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): (Stream[OpenInteraction[D, U]], Boolean) = {

    val (stream, maximal) = connector.getStableInteractionsWithout(waitingList, prohibited)

    val filteredStream = stream filter {
      case OpenInteraction(upwards, _, _) => predicate(upwards)
    }

    (filteredStream, maximal)
  }
}

/** Propagates the upwards value back during the downwards phase. */
private case class Feedback[D, U](
    connector: Connector[(D, U), U]) extends Connector[D, U] {

  override def getInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): Stream[OpenInteraction[D, U]] = {

    connector.getInteractionsWithout(waitingList, prohibited) map {
      case OpenInteraction(upwards, downwards, involved) =>
        OpenInteraction(upwards, (x: D) => downwards((x, upwards)), involved)
    }
  }

  override def getStableInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): (Stream[OpenInteraction[D, U]], Boolean) = {

    val (stream, maximal) = connector.getStableInteractionsWithout(waitingList, prohibited)

    val feedbackStream = stream map {
      case OpenInteraction(upwards, downwards, involved) =>
        OpenInteraction(upwards, (x: D) => downwards((x, upwards)), involved)
    }

    (feedbackStream, maximal)
  }
}

// Priority combinators

/** Gives priority to a connector. */
private case class FirstOf[D, U](
    left: Connector[D, U], 
    right: Connector[D, U]) extends Connector[D, U] {

  override def getInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): Stream[OpenInteraction[D, U]] = {

    val lefts = left.getInteractionsWithout(waitingList, Set())

    if (!lefts.isEmpty) {
      lefts.filter(_.involved.forall(!prohibited.contains(_)))
    }
    else {
      right.getInteractionsWithout(waitingList, prohibited)
    }
  }

  override def getStableInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): (Stream[OpenInteraction[D, U]], Boolean) = {

    val (lefts, maximal) =
      left.getStableInteractionsWithout(waitingList, Set())

    if (!lefts.isEmpty) {
      (lefts.filter(_.involved.forall(!prohibited.contains(_))), maximal)
    }
    else {
      right.getStableInteractionsWithout(waitingList, prohibited)
    }
  }
}

/** Ensures that upwards values are maximal according to some ordering. */
private case class Maximal[D, U](
    ordering: PartialOrdering[U],
    connector: Connector[D, U]) extends Connector[D, U] {

  override def getInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): Stream[OpenInteraction[D, U]] = {

    val interactions =
      connector.getInteractionsWithout(waitingList, Set()).toList

    val upwardsValues = interactions.map(_.upwards)

    def maximalAndPossible(o: OpenInteraction[D, U]): Boolean = o match {
      case OpenInteraction(upwards, _, involved) =>
        !upwardsValues.exists(ordering.gt(_, upwards)) &&
        involved.forall(!prohibited.contains(_))
    }

    interactions.filter(maximalAndPossible).toStream
  }

  override def getStableInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): (Stream[OpenInteraction[D, U]], Boolean) = {

    val (stream, maximal) =
      connector.getStableInteractionsWithout(waitingList, Set())

    if (!maximal) {
      return (Stream(), false)
    }

    val interactions = stream.toList

    val upwardsValues = interactions.map(_.upwards)

    def maximalAndPossible(o: OpenInteraction[D, U]): Boolean = o match {
      case OpenInteraction(upwards, _, involved) =>
        !upwardsValues.exists(ordering.gt(_, upwards)) &&
        involved.forall(!prohibited.contains(_))
    }

    (interactions.filter(maximalAndPossible).toStream, true)
  }
}

// Dynamic combinators

/** Accepts any atom on the given port. */
private case class Dynamic[D, U](port: Port[D, U]) extends Connector[D, U] {

  override def getInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): Stream[OpenInteraction[D, U]] = {

    waitingList.getBoundInteractions(port).filter(
      _.involved.forall(!prohibited.contains(_)))
  }

  override def getStableInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): (Stream[OpenInteraction[D, U]], Boolean) = {

    val stream = waitingList.getBoundInteractions(port).filter(
      _.involved.forall(!prohibited.contains(_)))

    (stream, false)
  }
}

/** Synchronizes with the connector provided as upwards value. */
private case class Joined[D, U](
    connector: Connector[D, Connector[D, U]]) extends Connector[D, U] {

  override def getInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): Stream[OpenInteraction[D, U]] = for {
    OpenInteraction(u1, d1, is1) <-
      connector.getInteractionsWithout(waitingList, prohibited)
    OpenInteraction(u2, d2, is2) <-
      u1.getInteractionsWithout(waitingList, prohibited union is1)
  } yield OpenInteraction(u2, (x: D) => d1(x) ++ d2(x), is1 union is2)

  override def getStableInteractionsWithout(
      waitingList: WaitingList,
      prohibited: Set[Atom]): (Stream[OpenInteraction[D, U]], Boolean) = {

    var maximal = true

    def removeMaximalTag[A](t: (A, Boolean)): A = {
      maximal &= t._2
      t._1
    }

    val stream = for {
      OpenInteraction(u1, d1, is1) <-
        removeMaximalTag(connector.getStableInteractionsWithout(
          waitingList, prohibited))
      OpenInteraction(u2, d2, is2) <-
        removeMaximalTag(u1.getStableInteractionsWithout(
          waitingList, prohibited union is1))
    } yield OpenInteraction(u2, (x: D) => d1(x) ++ d2(x), is1 union is2)

    (stream, maximal)
  }
}

package object connectors {

  /** Binds an atom to a port. */
  def bind[D, U](atom: Atom, port: Port[D, U]): Connector[D, U] =
    Bind(atom, port)

  /** Always provides the same upwards value. */
  def success[U](value: U): Connector[Any, U] = Success(value)

  /** Never provide any upwards value. */
  val failure: Connector[Any, Nothing] = Failure

  /** Disjunction of connectors. */
  def anyOf[D, U](connectors: Connector[D, U]*): Connector[D, U] = {
    val zero: Connector[D, U] = failure
    connectors.foldRight(zero)(OneOf(_, _))
  }

  /** Synchronization of connectors. */
  def allOf[D, U](connectors: Connector[D, U]*): Connector[D, Seq[U]] = {
    val one: Connector[D, Seq[U]] = success(Seq())
    def concat(t: (U, Seq[U])): Seq[U] = t._1 +: t._2
    connectors.foldRight(one) {
      case (first, rest) => Mapped(concat, BothOf(first, rest))
    }
  }

  /** Synchronization of any number of underlying connectors. */
  def manyOf[D, U](connectors: Connector[D, U]*): Connector[D, Seq[U]] = {
    allOf(connectors.map(_.map(Seq(_)).or(success(Seq()))) : _*).map(_.flatten)
  }

  /** Disjunction of connectors with priority. */
  def firstOf[D, U](connectors: Connector[D, U]*): Connector[D, U] = {
    val zero: Connector[D, U] = failure
    connectors.foldRight(zero)(FirstOf(_, _))
  }

  /** Binds any atom on the port. */
  def dynamic[D, U](port: Port[D, U]): Connector[D, U] = Dynamic(port)

  /** Uses the upwards value as the downwards value. */
  def close[D, U <: D](connector: Connector[D, U]): Connector[Any, Unit] = {
    val second = (t: (Any, U)) => t._2
    Feedback(connector.contramap(second)).sending(())
  }
}

/** Contains a connector whose upwards and downwards type coincide. */
private case class ClosedConnector[A](connector: Connector[A, A]) {

  /** Returns a stream of possible interactions. */
  def getInteractions(waitingList: WaitingList): Stream[Interaction] =
    connector.getInteractions(waitingList).map(Interaction.close)

  /** Returns a stream of possible stable interactions. */
  def getStableInteractions(waitingList: WaitingList): Stream[Interaction] =
    connector.getStableInteractions(waitingList).map(Interaction.close)
}