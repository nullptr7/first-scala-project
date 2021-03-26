package homegrown.collections

sealed trait OldSetV4[Element] extends (Element => Boolean) {

  final override def apply(input: Element): Boolean = {

    var result: Boolean = false
    foreach { current =>
      result = result || input == current
    }
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

  import OldSetV4._
  final def add(input: Element): OldSetV4[Element] = {

    var result: OldSetV4[Element] = NonEmpty(input, empty)
    foreach { current =>
      if (current != input) result = NonEmpty(current, result)
    }
    result
  }

  final def remove(input: Element): OldSetV4[Element] = {
    var result: OldSetV4[Element] = empty[Element]
    foreach { current =>
      // if (current != input) result = NonEmpty(current, result) ---- can also be done this way
      if (current != input) result = result.add(current)
    }
    result
  }

  final def union(that: OldSetV4[Element]): OldSetV4[Element] = {
    var result: OldSetV4[Element] = that
    foreach { current =>
      // if (!result(current)) result = NonEmpty(current, result) ---- can also be done this way
      if (!result(current)) result = result.add(current)
    }
    result
  }

  final def intersection(that: OldSetV4[Element]): OldSetV4[Element] = {
    var result: OldSetV4[Element] = empty[Element]
    foreach { current =>
      // if (that(current)) result = NonEmpty(current, result)
      if (that(current)) result = result.add(current)
    }
    result
  }

  final def difference(that: OldSetV4[Element]): OldSetV4[Element] = {
    var result: OldSetV4[Element] = empty[Element]
    foreach { current =>
      // if (!that(current)) result = NonEmpty(current, result)
      if (!that(current)) result = result.add(current)
    }
    result
  }

  final def isSubOldSetV4Of(that: OldSetV4[Element]): Boolean = {
    var result: Boolean = true
    foreach { current =>
      if (!that(current)) result = false
    }
    result
  }

  final def size: Int = {
    var result: Int = 0
    foreach(_ => result += 1)
    result
  }

  def isSingleton: Boolean = {
    if (isEmpty)
      false
    else {
      val aOldSetV4: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
      aOldSetV4.otherElements.isEmpty
    }
  }

  final override def hashCode: Int = {

    var result: Int = 41

    /* if (isEmpty)
      41
    else {
      val nonEmptyOldSetV4: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
      nonEmptyOldSetV4.element.hashCode + nonEmptyOldSetV4.otherElements.hashCode
    } */

    foreach(result += _.hashCode)

    result
  }

  final def sample: Option[Element] = {
    if (isEmpty) None
    else {
      val nonEmptyOldSetV4: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
      Some(nonEmptyOldSetV4.element)
    }
  }

  // final def isEmpty: Boolean = this eq empty //TODO: Needs to be changed as below
  final def isEmpty: Boolean = this.isInstanceOf[Empty[Element]]

  final def isNonEmpty: Boolean = !isEmpty

  final def isSuperOldSetV4Of(that: OldSetV4[Element]): Boolean = that.isSubOldSetV4Of(this)

  final override def equals(other: Any): Boolean = other match {
    case that: OldSetV4[Element] => this.isSubOldSetV4Of(that) && that.isSubOldSetV4Of(this)
    case _                       => false
  }

  final def foreach[Result](function: Element => Result): Unit = {
    if (isNonEmpty) {
      val nonEmptyOldSetV4: NonEmpty[Element] = this.asInstanceOf[NonEmpty[Element]]
      function(nonEmptyOldSetV4.element)
      nonEmptyOldSetV4.otherElements.foreach(function)
    }
  }

  final def map[Result](function: Element => Result): OldSetV4[Result] = {
    /* if (isEmpty)
      this
    else {
      val nonEmptyOldSetV4: NonEmpty = this.asInstanceOf[NonEmpty]
      val element = nonEmptyOldSetV4.element
      val otherElements = nonEmptyOldSetV4.otherElements
      NonEmpty(function(element), otherElements.map(function))
    } */
    var result: OldSetV4[Result] = empty[Result]
    foreach { current =>
      result = result.add(function(current))
    }
    result
  }

  /*
    This too can be used however not advisable since it has an overhead of adding
    a new everytime


    final def map[Result](function: Element => Result): OldSetV4[Result] = {
      flatmap { current =>
        OldSetV4[Result](function(current))
      }
    }
  */

  final def flatmap[Result](function: Element => OldSetV4[Result]): OldSetV4[Result] = {
    var result: OldSetV4[Result] = empty[Result]
    foreach { outerCurrent =>
      function(outerCurrent).foreach { innerCurrent =>
        result = result.add(innerCurrent)
      }
    }
    result
  }
}

object OldSetV4 {

  def apply[Element](firstElement: Element, otherElements: Element*): OldSetV4[Element] = {
    var result: OldSetV4[Element] = empty[Element].add(firstElement)
    otherElements.foreach { current =>
      result = result.add(current)
    }
    result
  }

  private final case class NonEmpty[Element](element: Element, otherElements: OldSetV4[Element]) extends OldSetV4[Element]

  private object NonEmpty {
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
  }

  private class Empty[Element] extends OldSetV4[Element] {
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
  }

  private[this] def unapply[Element](any: Any): Option[(Element, Any)] = patternMatchingNotSupported

  private[this] def patternMatchingNotSupported: Nothing = sys.error("Pattern matching is not supported in set and really expensive")

  def empty[Element]: OldSetV4[Element] = new Empty[Element]
}
