package homegrown.collections

trait OldSet extends (String => Boolean) {
  final def remove(input: String): OldSet = element => {

    //println(s"input($input) & element($element)")
    input != element && this(input)
  }

  final def add(input: String): OldSet = element => {
    //println(s"input($input) & element($element)")
    input == element || this(element)
  }

  final def union(that: OldSet): OldSet = element =>
    this(element) || that(element)

  final def intersection(that: OldSet): OldSet = element =>
    this(element) && that(element)

  final def difference(that: OldSet): OldSet = element =>
    this(element) && !that(element)

  final def isSubOldSetOf(that: OldSet): Boolean = ???

}

object OldSet {
  //val empty: OldSet = input => false

  ///above line is converted as below
  val empty: OldSet = new OldSet {
    override final def apply(input: String): Boolean = {
      false
    }
  }

}
