package homegrown.collections

sealed trait OldSetV5[Element] extends (Element => Boolean) {

  final override def apply(input: Element): Boolean = {

    /* val seed: Boolean = false

    var acc: Boolean = seed
    foreach { current =>
      acc = acc || input == current
    }

    val result: Boolean = acc

    result */

    // fold(seed = false, function: (Boolean, Element) => Boolean)

    val result =
      fold[Boolean](seed = false)(_ || _ == input)
    /*
        Above code is more concise
        fold(seed = false) { (acc, current) =>
          acc || input == current
        }
      */

    result
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

  import OldSetV5._

  @scala.annotation.tailrec
  final def fold[Result](seed: Result)(function: (Result, Element) => Result): Result = {

    if (isEmpty)
      seed
    else {
      val nonEmptyOldSetV5: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
      val element: Element = nonEmptyOldSetV5.element
      val otherElements: OldSetV5[Element] = nonEmptyOldSetV5.otherElements
      otherElements.fold(seed = function(seed, element))(function)
    }
  }

  final def add(input: Element): OldSetV5[Element] = {

    fold[NonEmpty[Element]](seed = NonEmpty(input, empty)) { (acc, current) =>
      if (current != input)
        NonEmpty(current, acc)
      else
        acc
    }

    /* val seed: OldSetV5[Element] = NonEmpty(input, empty)

    var acc: OldSetV5[Element] = seed

    foreach { current =>
      if (current != input) acc = NonEmpty(current, acc)
    }

    val result: OldSetV5[Element] = acc
    result */
  }

  final def remove(input: Element): OldSetV5[Element] = {

    fold[OldSetV5[Element]](seed = empty[Element]) { (acc, current) =>
      if (current == input) acc
      else acc.add(current)
    }

    /* var result: OldSetV5[Element] = empty[Element]
    foreach { current =>
      // if (current != input) result = NonEmpty(current, result) ---- can also be done this way
      if (current != input) result = result.add(current)
    }
    result */
  }

  final def union(that: OldSetV5[Element]): OldSetV5[Element] = {

    // Shorthand version below
    fold(that)(_ add _)

    /*
      fold[OldSetV5[Element]](seed = that) { (acc, current) =>
        acc.add(current)
      }
    */

    /*
      var result: OldSetV5[Element] = that
      foreach { current =>
        // if (!result(current)) result = NonEmpty(current, result) ---- can also be done this way
        if (!result(current)) result = result.add(current)
      }
      result
    */
  }

  final def intersection(that: OldSetV5[Element]): OldSetV5[Element] = {

    fold[OldSetV5[Element]](seed = empty[Element]) { (acc, current) =>
      if (that(current)) acc.add(current)
      else acc
    }

    /*
      var result: OldSetV5[Element] = empty[Element]
      foreach { current =>
        // if (that(current)) result = NonEmpty(current, result)
        if (that(current)) result = result.add(current)
      }
      result
    */
  }

  final def difference(that: OldSetV5[Element]): OldSetV5[Element] = {

    fold[OldSetV5[Element]](seed = empty[Element]) { (acc, current) =>
      if (that(current)) acc
      else acc.add(current)
    }

    /*
      var result: OldSetV5[Element] = empty[Element]
      foreach { current =>
        // if (!that(current)) result = NonEmpty(current, result)
        if (!that(current)) result = result.add(current)
      }
      result
    */
  }

  final def isSubOldSetV5Of(that: OldSetV5[Element]): Boolean = {

    fold(true)(_ && that(_))

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

  def isSingleton: Boolean = {
    if (isEmpty)
      false
    else {
      val aOldSetV5: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
      aOldSetV5.otherElements.isEmpty
    }
  }

  final override def hashCode: Int = {

    fold[Int](41)(_ + _.hashCode)

    // fold[Int](seed = 41) { (acc, current) => acc + current.hashCode }

    /* var result: Int = 41

    /* if (isEmpty)
      41
    else {
      val nonEmptyOldSetV5: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
      nonEmptyOldSetV5.element.hashCode + nonEmptyOldSetV5.otherElements.hashCode
    } */

    foreach(result += _.hashCode)

    result */
  }

  final def sample: Option[Element] = {
    if (isEmpty) None
    else {
      val nonEmptyOldSetV5: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
      Some(nonEmptyOldSetV5.element)
    }
  }

  // final def isEmpty: Boolean = this eq empty //TODO: Needs to be changed as below
  final def isEmpty: Boolean = this.isInstanceOf[Empty[Element]]

  final def isNonEmpty: Boolean = !isEmpty

  final def isSuperOldSetV5Of(that: OldSetV5[Element]): Boolean = that.isSubOldSetV5Of(this)

  final override def equals(other: Any): Boolean = other match {
    case that: OldSetV5[Element] => this.isSubOldSetV5Of(that) && that.isSubOldSetV5Of(this)
    case _                       => false
  }

  final def foreach[Result](function: Element => Result): Unit = {
    /*
      if (isNonEmpty) {
        val nonEmptyOldSetV5: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
        function(nonEmptyOldSetV5.element)
        nonEmptyOldSetV5.otherElements.foreach(function)
      }
    */

    fold[Unit](()) { (_, current) => function(current) }
  }

  final def map[Result](function: Element => Result): OldSetV5[Result] = {

    fold(empty[Result])(_ add function(_))

    /*
      fold[OldSetV5[Result]](seed = empty[Result]) { (acc, current) =>
        acc.add(function(current))
      }
    */

    /*

    /* if (isEmpty)
      this
    else {
      val nonEmptyOldSetV5: NonEmpty = this.asInstanceOf[NonEmpty]
      val element = nonEmptyOldSetV5.element
      val otherElements = nonEmptyOldSetV5.otherElements
      NonEmpty(function(element), otherElements.map(function))
    } */


    var result: OldSetV5[Result] = empty[Result]
    foreach { current =>
      result = result.add(function(current))
    }
    result
    */
  }

  /*
    This too can be used however not advisable since it has an overhead of adding
    a new everytime


    final def map[Result](function: Element => Result): OldSetV5[Result] = {
      flatmap { current =>
        OldSetV5[Result](function(current))
      }
    }
  */

  final def flatmap[Result](function: Element => OldSetV5[Result]): OldSetV5[Result] = {

    fold[OldSetV5[Result]](seed = empty[Result]) { (acc, outerCurrent) =>
      function(outerCurrent).fold(acc)(_ add _)
    }

    /* Better version above
      fold[OldSetV5[Result]](seed = empty[Result]) { (acc, outerCurrent) =>
        function(outerCurrent).fold(seed = acc) { (acc1, innerCurrent) =>
          acc1.add(innerCurrent)
        }
      }
    */

    /* var result: OldSetV5[Result] = empty[Result]
    foreach { outerCurrent =>
      function(outerCurrent).foreach { innerCurrent =>
        result = result.add(innerCurrent)
      }
    }
    result */
  }
}

object OldSetV5 {

  def apply[Element](firstElement: Element, otherElements: Element*): OldSetV5[Element] = {

    otherElements.foldLeft(empty[Element].add(firstElement))(_ add _)

    /* Better version is using foldLeft as above
      var result: OldSetV5[Element] = empty[Element].add(firstElement)

      otherElements.foreach { current =>
        result = result.add(current)
      }
      result
    */
  }

  private final case class NonEmpty[Element](element: Element, otherElements: OldSetV5[Element]) extends OldSetV5[Element]

  private object NonEmpty {
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
  }

  private class Empty[Element] extends OldSetV5[Element] {
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
  }

  private[this] def unapply[Element](any: Any): Option[(Element, Any)] = patternMatchingNotSupported

  private[this] def patternMatchingNotSupported: Nothing = sys.error("Pattern matching is not supported in set and really expensive")

  def empty[Element]: OldSetV5[Element] = new Empty[Element]
}
