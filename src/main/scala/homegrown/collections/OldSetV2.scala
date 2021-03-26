package homegrown.collections

sealed trait OldSetV2 extends (String => Boolean) {
  def add(input: String): OldSetV2

  def remove(input: String): OldSetV2

  def union(that: OldSetV2): OldSetV2

  def intersection(that: OldSetV2): OldSetV2

  def difference(that: OldSetV2): OldSetV2

  def isSubOldSetV2Of(that: OldSetV2): Boolean

  def size: Int

  def isSingleton: Boolean

  def sample: Option[String]

  def foreach(function: String => Unit): Unit

  final def isEmpty: Boolean = this eq OldSetV2.empty

  final def isNonEmpty: Boolean = !isEmpty

  final def isSuperOldSetV2Of(that: OldSetV2): Boolean = that.isSubOldSetV2Of(this)

  final override def equals(other: Any): Boolean = other match {
    case that: OldSetV2 => this.isSubOldSetV2Of(that) && that.isSubOldSetV2Of(this)
    case _              => false
  }

}

object OldSetV2 {

  def apply(firstElement: String, otherElements: String*): OldSetV2 = {
    var result: OldSetV2 = empty.add(firstElement)
    otherElements.foreach { current =>
      result = result.add(current)
    }
    result
  }

  private final case class NonEmpty(element: String, otherElements: OldSetV2)
    extends OldSetV2 {

    final override def remove(input: String): OldSetV2 = {
      if (input == element) otherElements
      else NonEmpty(element, otherElements.remove(input))
    }

    final override def apply(input: String): Boolean = {
      input == element || otherElements(input)
    }
    final override def add(input: String): OldSetV2 = {
      if (input == element) this
      else NonEmpty(input, otherElements.add(element))
    }

    final override def union(that: OldSetV2): OldSetV2 =
      otherElements.union(that.add(element))

    final override def intersection(that: OldSetV2): OldSetV2 = {

      val intersectionWithOthers: OldSetV2 = otherElements.intersection(that)

      if (that(element)) {
        intersectionWithOthers.add(element)
      }
      else {
        intersectionWithOthers
      }
    }

    final override def difference(that: OldSetV2): OldSetV2 = {
      val differenceWithOther: OldSetV2 = otherElements.difference(that)
      if (that(element)) {
        differenceWithOther
      }
      else {
        differenceWithOther.add(element)
      }
    }

    final override def isSubOldSetV2Of(that: OldSetV2): Boolean = {
      that(element) && otherElements.isSubOldSetV2Of(that)
    }

    final override def hashCode: Int =
      element.hashCode + otherElements.hashCode

    final override def size: Int = {
      1 + otherElements.size
    }

    final override def isSingleton: Boolean =
      otherElements eq OldSetV2.empty
    // or
    //otherElements.isEmpty

    final override def sample: Option[String] = Some(element)

    final override def foreach(function: String => Unit): Unit = {
      function(element)
      otherElements.foreach(function)
    }
  }

  private object Empty extends OldSetV2 {

    final override def remove(input: String): OldSetV2 = this

    final override def apply(input: String): Boolean = false

    final override def add(input: String): OldSetV2 = NonEmpty(input, Empty)

    final override def union(that: OldSetV2): OldSetV2 = that

    final override def intersection(that: OldSetV2): OldSetV2 = this

    final override def difference(that: OldSetV2): OldSetV2 = this

    final override def isSubOldSetV2Of(that: OldSetV2): Boolean = true

    final override def size: Int = 0

    final override def isSingleton: Boolean = false

    final override def sample: Option[String] = None

    final override def foreach(function: String => Unit): Unit = ()
  }

  val empty: OldSetV2 = Empty
}
