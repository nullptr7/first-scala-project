package homegrown.collections

sealed trait OldSetV6[Element] extends (Element => Boolean) {

  //This basicaly acts as a contains where if we say as val set:OldSetV6[String] = OldSetV6("1")
  // set("1") will give back true
  final override def apply(input: Element): Boolean = {

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

  /*
    final override def applyOld(input: Element): Boolean = {
      var result: Boolean = false
      foreach { current =>
        result = result || input == current
      }
      result
    }
  */

  final def contains(that: Element): Boolean =
    // fold[Boolean](seed = false)(_ || _ == that)
    // can also be written as below
    exists(_ == that)

  final def doesNotContain(that: Element): Boolean = !contains(that)

  final def doesNotExists(predicate: Element => Boolean): Boolean = !exists(predicate)

  final def exists(predicate: Element => Boolean): Boolean = fold(seed = false)(_ || predicate(_))

  final def forall(predicate: Element => Boolean): Boolean =
    fold[Boolean](seed = true)(_ && predicate(_))

  final def notForAll(predicate: Element => Boolean): Boolean =
    !forall(predicate)

  import OldSetV6._

  final def add(input: Element): OldSetV6[Element] = {

    fold[NonEmpty[Element]](seed = NonEmpty(input, empty)) { (acc, current) =>
      if (current != input)
        NonEmpty(current, acc)
      else
        acc
    }

    /* val seed: OldSetV6[Element] = NonEmpty(input, empty)

    var acc: OldSetV6[Element] = seed

    foreach { current =>
      if (current != input) acc = NonEmpty(current, acc)
    }

    val result: OldSetV6[Element] = acc
    result */
  }

  final def remove(input: Element): OldSetV6[Element] = {

    fold[OldSetV6[Element]](seed = empty[Element]) { (acc, current) =>
      if (current == input) acc
      else acc.add(current)
    }

    /* var result: OldSetV6[Element] = empty[Element]
    foreach { current =>
      // if (current != input) result = NonEmpty(current, result) ---- can also be done this way
      if (current != input) result = result.add(current)
    }
    result */
  }

  final def union(that: OldSetV6[Element]): OldSetV6[Element] = {

    // Shorthand version below
    fold(that)(_ add _)

    /*
      fold[OldSetV6[Element]](seed = that) { (acc, current) =>
        acc.add(current)
      }
    */

    /*
      var result: OldSetV6[Element] = that
      foreach { current =>
        // if (!result(current)) result = NonEmpty(current, result) ---- can also be done this way
        if (!result(current)) result = result.add(current)
      }
      result
    */
  }

  // Can be changed from OldSetV6[Element] to Element => Boolean
  final def intersection(that: Element => Boolean): OldSetV6[Element] = {

    filter(that)
    /*
      fold[OldSetV6[Element]](seed = empty[Element]) { (acc, current) =>
        if (that(current)) acc.add(current)
        else acc
      }
    */

    /*
      var result: OldSetV6[Element] = empty[Element]
      foreach { current =>
        // if (that(current)) result = NonEmpty(current, result)
        if (that(current)) result = result.add(current)
      }
      result
      */
  }

  final def filter(predicate: Element => Boolean): OldSetV6[Element] =
    fold[OldSetV6[Element]](seed = empty[Element]) { (acc, current) =>
      if (predicate(current)) acc.add(current)
      else acc
    }

  // Can be changed from OldSetV6[Element] to Element => Boolean
  final def difference(predicate: Element => Boolean): OldSetV6[Element] = {

    fold[OldSetV6[Element]](seed = empty[Element]) { (acc, current) =>
      if (predicate(current)) acc
      else acc.add(current)
    }

    /*
      var result: OldSetV6[Element] = empty[Element]
      foreach { current =>
        // if (!that(current)) result = NonEmpty(current, result)
        if (!that(current)) result = result.add(current)
      }
      result
      */
  }

  // Can be changed from OldSetV6[Element] to Element => Boolean
  final def isSubOldSetV6Of(predicate: Element => Boolean): Boolean = {

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

  /*

    def isSingleton: Boolean = {
      if (isEmpty)
        false
      else {
        /*
          val aOldSetV6: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
          aOldSetV6.otherElements.isEmpty
        */

        otherElementOrThrowException.isEmpty
      }
    }
  */

  def isSingleton: Boolean = isNonEmpty && otherElementOrThrowException.isEmpty

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
    case that: OldSetV6[Element] => this.isSubOldSetV6Of(that) && that.isSubOldSetV6Of(this)
    case _                       => false
  }

  final override def hashCode: Int = {

    fold[Int](41)(_ + _.hashCode)

    // fold[Int](seed = 41) { (acc, current) => acc + current.hashCode }

    /* var result: Int = 41

    /* if (isEmpty)
      41
    else {
      val nonEmptyOldSetV6: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
      nonEmptyOldSetV6.element.hashCode + nonEmptyOldSetV6.otherElements.hashCode
    } */

    foreach(result += _.hashCode)

    result */
  }

  final def sample: Option[Element] = {
    if (isEmpty) None
    else {
      // val nonEmptyOldSetV6: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
      // Some(nonEmptyOldSetV6.element)
      Some(elementOrThrowException)
    }
  }

  // final def isEmpty: Boolean = this eq empty //TODO: Needs to be changed as below
  final def isEmpty: Boolean = this.isInstanceOf[Empty[Element]]

  final def isNonEmpty: Boolean = !isEmpty

  final def isSuperOldSetV6Of(that: OldSetV6[Element]): Boolean = that.isSubOldSetV6Of(this)

  final def foreach[Result](function: Element => Result): Unit = {
    /*
      if (isNonEmpty) {
        val nonEmptyOldSetV6: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
        function(nonEmptyOldSetV6.element)
        nonEmptyOldSetV6.otherElements.foreach(function)
      }
    */

    fold[Unit](()) { (_, current) => function(current) }
  }

  final def map[Result](function: Element => Result): OldSetV6[Result] = {

    fold(empty[Result])(_ add function(_))

    /*
      fold[OldSetV6[Result]](seed = empty[Result]) { (acc, current) =>
        acc.add(function(current))
      }
    */

    /*

    /* if (isEmpty)
      this
    else {
      val nonEmptyOldSetV6: NonEmpty = this.asInstanceOf[NonEmpty]
      val element = nonEmptyOldSetV6.element
      val otherElements = nonEmptyOldSetV6.otherElements
      NonEmpty(function(element), otherElements.map(function))
    } */


    var result: OldSetV6[Result] = empty[Result]
    foreach { current =>
      result = result.add(function(current))
    }
    result
    */
  }

  /*
    This too can be used however not advisable since it has an overhead of adding
    a new everytime


    final def map[Result](function: Element => Result): OldSetV6[Result] = {
      flatmap { current =>
        OldSetV6[Result](function(current))
      }
    }
  */

  final def flatmap[Result](function: Element => OldSetV6[Result]): OldSetV6[Result] = {

    fold[OldSetV6[Result]](seed = empty[Result]) { (acc, outerCurrent) =>
      function(outerCurrent).fold(acc)(_ add _)
    }

    /* Better version above
      fold[OldSetV6[Result]](seed = empty[Result]) { (acc, outerCurrent) =>
        function(outerCurrent).fold(seed = acc) { (acc1, innerCurrent) =>
          acc1.add(innerCurrent)
        }
      }
    */

    /* var result: OldSetV6[Result] = empty[Result]
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
      val nonEmptyOldSetV6: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
        val element: Element = nonEmptyOldSetV6.element
        val otherElements: OldSetV6[Element] = nonEmptyOldSetV6.otherElements
        otherElements.fold(seed = function(seed, element))(function)
      */

      otherElementOrThrowException.fold(function(seed, elementOrThrowException))(function)
    }
  }

  private[this] lazy val (elementOrThrowException, otherElementOrThrowException) = {

    val nonEmptyOldSetV6 = this.asInstanceOf[NonEmpty[Element]]
    val element: Element = nonEmptyOldSetV6.element
    val otherElement: OldSetV6[Element] = nonEmptyOldSetV6.otherElements

    element -> otherElement

  }
}

object OldSetV6 {

  def apply[Element](firstElement: Element, otherElements: Element*): OldSetV6[Element] = {

    otherElements.foldLeft(empty[Element].add(firstElement))(_ add _)

    /* Better version is using foldLeft as above
      var result: OldSetV6[Element] = empty[Element].add(firstElement)

      otherElements.foreach { current =>
        result = result.add(current)
      }
      result
    */
  }

  private final case class NonEmpty[Element](element: Element, otherElements: OldSetV6[Element]) extends OldSetV6[Element]

  private object NonEmpty {
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
  }

  private class Empty[Element] extends OldSetV6[Element] {
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
  }

  private[this] def unapply[Element](any: Any): Option[(Element, Any)] = patternMatchingNotSupported

  private[this] def patternMatchingNotSupported: Nothing = sys.error("Pattern matching is not supported in set and really expensive")

  def empty[Element]: OldSetV6[Element] = new Empty[Element]
}
