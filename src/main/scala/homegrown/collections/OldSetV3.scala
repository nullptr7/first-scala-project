package homegrown.collections

sealed trait OldSetV3 extends (String => Boolean) {

  final def apply(input: String): Boolean = {
    var result: Boolean = false
    foreach { current =>
      result = result || input == current
    }
    result
  }

  import OldSetV3._
  final def add(input: String): OldSetV3 = {
    var result: OldSetV3 = NonEmpty(input, Empty)
    foreach { current =>
      if (current != input) result = NonEmpty(current, result)
    }
    result
  }

  final def remove(input: String): OldSetV3 = {
    var result: OldSetV3 = OldSetV3.empty
    foreach { current =>
      // if (current != input) result = NonEmpty(current, result) ---- can also be done this way
      if (current != input) result = result.add(current)
    }
    result
  }

  final def union(that: OldSetV3): OldSetV3 = {
    var result: OldSetV3 = that
    foreach { current =>
      // if (!result(current)) result = NonEmpty(current, result) ---- can also be done this way
      if (!result(current)) result = result.add(current)
    }
    result
  }

  final def intersection(that: OldSetV3): OldSetV3 = {
    var result: OldSetV3 = OldSetV3.empty
    foreach { current =>
      // if (that(current)) result = NonEmpty(current, result)
      if (that(current)) result = result.add(current)
    }
    result
  }

  final def difference(that: OldSetV3): OldSetV3 = {
    var result: OldSetV3 = OldSetV3.empty
    foreach { current =>
      // if (!that(current)) result = NonEmpty(current, result)
      if (!that(current)) result = result.add(current)
    }
    result
  }

  final def isSubOldSetV3Of(that: OldSetV3): Boolean = {
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
      val aOldSetV3: NonEmpty = this.asInstanceOf[NonEmpty]
      aOldSetV3.otherElements.isEmpty
    }
  }

  final override def hashCode: Int = {
    if (isEmpty)
      41
    else {
      val nonEmptyOldSetV3: NonEmpty = this.asInstanceOf[NonEmpty]
      nonEmptyOldSetV3.element.hashCode + nonEmptyOldSetV3.otherElements.hashCode
    }
  }

  final def sample: Option[String] = {
    if (isEmpty) None
    else {
      val nonEmptyOldSetV3: NonEmpty = this.asInstanceOf[NonEmpty]
      Some(nonEmptyOldSetV3.element)
    }
  }

  final def isEmpty: Boolean = this eq OldSetV3.empty

  final def isNonEmpty: Boolean = !isEmpty

  final def isSuperOldSetV3Of(that: OldSetV3): Boolean = that.isSubOldSetV3Of(this)

  final override def equals(other: Any): Boolean = other match {
    case that: OldSetV3 => this.isSubOldSetV3Of(that) && that.isSubOldSetV3Of(this)
    case _              => false
  }

  final def foreach(function: String => Unit): Unit = {
    if (isNonEmpty) {

      val nonEmptyOldSetV3: NonEmpty = this.asInstanceOf[NonEmpty]

      function(nonEmptyOldSetV3.element)
      nonEmptyOldSetV3.otherElements.foreach(function)

      // Expensive operation hence not advisable and hence we have made un-compilable check unapply method
      // De-construction is better than pattern matching in this case
      /*
        this match {
          case NonEmpty(element, otherElements) =>
            function(element)
            otherElements.foreach(function)
          case _ => ()
        }
      */

      /*
        Can also be written as below
        val NonEmpty(element, otherElements) = this
        function(element)
        otherElements.foreach(function)
      */
    }
  }

}

object OldSetV3 {

  def apply(firstElement: String, otherElements: String*): OldSetV3 = {
    var result: OldSetV3 = empty.add(firstElement)
    otherElements.foreach { current =>
      result = result.add(current)
    }
    result
  }

  private final case class NonEmpty(element: String, otherElements: OldSetV3) extends OldSetV3

  private object NonEmpty {
    private[this] def unapply(any: Any): Option[(String, OldSetV3)] = patternMatchingNotSupported
  }

  private object Empty extends OldSetV3 {
    private[this] def unapply(any: Any): Option[(String, OldSetV3)] = patternMatchingNotSupported
  }

  private[this] def unapply(any: Any): Option[(String, OldSetV3)] = patternMatchingNotSupported

  private[this] def patternMatchingNotSupported: Nothing = sys.error("Pattern matching is not supported in set and really expensive")

  val empty: OldSetV3 = Empty
}
