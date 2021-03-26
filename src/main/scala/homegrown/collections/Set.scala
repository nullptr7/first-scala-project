package homegrown.collections

import homegrown.collections.Set.NonEmpty

import scala.language.implicitConversions

// We removed Element => Boolean function because it conflicted with
// variance covariant & contravariant cannot co-exist together.
sealed trait Set[+Element] {

  private[this] lazy val (elementOrThrowException, otherElementOrThrowException) = {

    val nonEmptySet: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
    val element: Element = nonEmptySet.element
    val otherElement: Set[Element] = nonEmptySet.otherElements

    element -> otherElement

  }

  /*
    final override def applyOld(input: Element): Boolean = {
      var result: Boolean = false
      foreach { current =>
        result = result || input == current
      }
      result
    }
  */

  final override def toString: String = {
    if (isEmpty)
      "{}"
    else {
      val otherElementsWithCommaSpace =
        otherElementOrThrowException.fold("") { (acc, current) =>
          s"$acc, $current"
        }
      "{" + elementOrThrowException + otherElementsWithCommaSpace + "}"
    }
  }

  final override def equals(other: Any): Boolean = other match {
    case that: Set[Element] => this.isSubSetOf(that) && that.isSubSetOf(this)
    case _                  => false
  }

  final override def hashCode: Int = {

    fold[Int](41)(_ + _.hashCode)

    // fold[Int](seed = 41) { (acc, current) => acc + current.hashCode }

    /* var result: Int = 41

    /* if (isEmpty)
      41
    else {
      val nonEmptySet: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
      nonEmptySet.element.hashCode + nonEmptySet.otherElements.hashCode
    } */

    foreach(result += _.hashCode)

    result */
  }

  //This basicaly acts as a contains where if we say as val set:Set[String] = Set("1")
  // set("1") will give back true
  final def apply[Super >: Element](input: Super): Boolean = {

    /* val seed: Boolean = false

    var acc: Boolean = seed
    foreach { current =>
      acc = acc || input == current
    }

    val result: Boolean = acc

    result */

    // fold(seed = false, function: (Boolean, Element) => Boolean)

    /*
      val result = fold[Boolean](seed = false)(_ || _ == input)
      result
    */
    /*
        Above code is more concise
        fold(seed = false) { (acc, current) =>
          acc || input == current
        }
      */

    contains(input)

  }

  final def contains[Super >: Element](that: Super): Boolean =
    // fold[Boolean](seed = false)(_ || _ == that)
    // can also be written as below
    exists(_ == that)

  final def doesNotContain[Super >: Element](that: Super): Boolean = !contains(that)

  import Set._

  final def doesNotExists(predicate: Element => Boolean): Boolean = !exists(predicate)

  final def exists(predicate: Element => Boolean): Boolean = fold(seed = false)(_ || predicate(_))

  final def forall(predicate: Element => Boolean): Boolean = fold[Boolean](seed = true)(_ && predicate(_))

  final def notForAll(predicate: Element => Boolean): Boolean = !forall(predicate)

  final def add[Super >: Element](input: Super): Set[Super] = {
    fold[NonEmpty[Super]](seed = NonEmpty(input, empty)) { (acc, current) =>
      if (current != input)
        NonEmpty(current, acc)
      else
        acc
    }

    /* val seed: Set[Element] = NonEmpty(input, empty)

    var acc: Set[Element] = seed

    foreach { current =>
      if (current != input) acc = NonEmpty(current, acc)
    }

    val result: Set[Element] = acc
    result */
  }

  final def remove[Super >: Element](input: Super): Set[Super] = {

    fold[Set[Super]](seed = empty[Super]) { (acc, current) =>
      if (current == input) acc
      else acc.add(current)
    }

    /* var result: Set[Element] = empty[Element]
    foreach { current =>
      // if (current != input) result = NonEmpty(current, result) ---- can also be done this way
      if (current != input) result = result.add(current)
    }
    result */
  }

  final def union[Super >: Element](that: Set[Super]): Set[Super] = {

    // Shorthand version below
    fold(that)(_ add _)

    /*
      fold[Set[Element]](seed = that) { (acc, current) =>
        acc.add(current)
      }
    */

    /*
      var result: Set[Element] = that
      foreach { current =>
        // if (!result(current)) result = NonEmpty(current, result) ---- can also be done this way
        if (!result(current)) result = result.add(current)
      }
      result
    */
  }

  // Can be changed from Set[Element] to Element => Boolean
  final def intersection(that: Element => Boolean): Set[Element] = {

    filter(that)
    /*
      fold[Set[Element]](seed = empty[Element]) { (acc, current) =>
        if (that(current)) acc.add(current)
        else acc
      }
    */

    /*
      var result: Set[Element] = empty[Element]
      foreach { current =>
        // if (that(current)) result = NonEmpty(current, result)
        if (that(current)) result = result.add(current)
      }
      result
      */
  }

  /*

    def isSingleton: Boolean = {
      if (isEmpty)
        false
      else {
        /*
          val aSet: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
          aSet.otherElements.isEmpty
        */

        otherElementOrThrowException.isEmpty
      }
    }
  */

  final def filter(predicate: Element => Boolean): Set[Element] =
    fold[Set[Element]](seed = empty[Element]) { (acc, current) =>
      if (predicate(current)) acc.add(current)
      else acc
    }

  // Can be changed from Set[Element] to Element => Boolean
  final def difference(predicate: Element => Boolean): Set[Element] = {

    fold[Set[Element]](seed = empty[Element]) { (acc, current) =>
      if (predicate(current)) acc
      else acc.add(current)
    }

    /*
      var result: Set[Element] = empty[Element]
      foreach { current =>
        // if (!that(current)) result = NonEmpty(current, result)
        if (!that(current)) result = result.add(current)
      }
      result
      */
  }

  // Can be changed from Set[Element] to Element => Boolean
  final def isSubSetOf(predicate: Element => Boolean): Boolean = {

    forall(predicate)
    // Forall has same body
    //fold(true)(_ && predicate(_))

    /*  //Better version is above =
      fold[Boolean](seed = true) { (acc, current) =>
        if (acc && that(current)) true
        else false
      }
    */

    /*
      var result: Boolean = true
      foreach { current =>
        if (!that(current)) result = false
      }
      result
    */
  }

  final def size: Int = {

    fold[Int](seed = 0) { (acc, _) => acc + 1 }

    /*
      var result: Int = 0
      foreach(_ => result += 1)
      result
    */
  }

  def isSingleton: Boolean = isNonEmpty && otherElementOrThrowException.isEmpty

  final def isNonEmpty: Boolean = !isEmpty

  final def sample: Option[Element] = {
    if (isEmpty) None
    else {
      // val nonEmptySet: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
      // Some(nonEmptySet.element)
      Some(elementOrThrowException)
    }
  }

  // final def isEmpty: Boolean = this eq empty //TODO: Needs to be changed as below
  final def isEmpty: Boolean = this.isInstanceOf[Empty.type]

  final def isSuperSetOf[Super >: Element](that: Set[Super]): Boolean = that.isSubSetOf(this)

  final def foreach[Result](function: Element => Result): Unit = {
    /*
      if (isNonEmpty) {
        val nonEmptySet: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
        function(nonEmptySet.element)
        nonEmptySet.otherElements.foreach(function)
      }
    */

    fold[Unit](()) { (_, current) => function(current) }
  }

  /*
    This too can be used however not advisable since it has an overhead of adding
    a new everytime


    final def map[Result](function: Element => Result): Set[Result] = {
      flatmap { current =>
        Set[Result](function(current))
      }
    }
  */

  final def map[Result](function: Element => Result): Set[Result] = {

    fold(empty[Result])(_ add function(_))

    /*
      fold[Set[Result]](seed = empty[Result]) { (acc, current) =>
        acc.add(function(current))
      }
    */

    /*

    /* if (isEmpty)
      this
    else {
      val nonEmptySet: NonEmpty = this.asInstanceOf[NonEmpty]
      val element = nonEmptySet.element
      val otherElements = nonEmptySet.otherElements
      NonEmpty(function(element), otherElements.map(function))
    } */


    var result: Set[Result] = empty[Result]
    foreach { current =>
      result = result.add(function(current))
    }
    result
    */
  }

  final def flatmap[Result](function: Element => Set[Result]): Set[Result] = {

    fold[Set[Result]](seed = empty[Result]) { (acc, outerCurrent) =>
      function(outerCurrent).fold(acc)(_ add _)
    }

    /* Better version above
      fold[Set[Result]](seed = empty[Result]) { (acc, outerCurrent) =>
        function(outerCurrent).fold(seed = acc) { (acc1, innerCurrent) =>
          acc1.add(innerCurrent)
        }
      }
    */

    /* var result: Set[Result] = empty[Result]
    foreach { outerCurrent =>
      function(outerCurrent).foreach { innerCurrent =>
        result = result.add(innerCurrent)
      }
    }
    result */
  }

  @scala.annotation.tailrec
  final def fold[Result](seed: Result)(function: (Result, Element) => Result): Result = {

    if (isEmpty)
      seed
    else {
      /*
      val nonEmptySet: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
        val element: Element = nonEmptySet.element
        val otherElements: Set[Element] = nonEmptySet.otherElements
        otherElements.fold(seed = function(seed, element))(function)
      */

      otherElementOrThrowException.fold(function(seed, elementOrThrowException))(function)
    }
  }
}

object Set {

  def apply[Element](firstElement: Element, otherElements: Element*): Set[Element] = {

    otherElements.foldLeft(empty[Element].add(firstElement))(_ add _)

    /* Better version is using foldLeft as above
      var result: Set[Element] = empty[Element].add(firstElement)

      otherElements.foreach { current =>
        result = result.add(current)
      }
      result
    */
  }

  def empty[Element]: Set[Element] = Empty

  private[this] def unapply[Element](any: Any): Option[(Element, Any)] = patternMatchingNotSupported

  private[this] def patternMatchingNotSupported: Nothing =
    sys.error("Pattern matching is not supported in set and really expensive")

  private final case class NonEmpty[Element](element: Element, otherElements: Set[Element]) extends Set[Element]

  private object NonEmpty {
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
  }

  private object Empty extends Set[Nothing] {
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
  }

  implicit def convertSetToFunction[Element](set: Set[Element]): Element => Boolean =
    set.apply
}
