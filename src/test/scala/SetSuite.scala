import homegrown.collections._
import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers

class SetSuite extends AnyFunSuite with Matchers {

  test("apply on an empty set should yield false") {
    Set.empty[String](randomString) shouldBe false
  }

  test("Add on an empty set should yield a new set with one element") {
    val first: String = randomString
    val second: String = randomString

    first should not be second
    val set: Set[String] = Set(first, second)

    set(first) shouldBe true
    set(randomString) shouldBe false
    set(second) shouldBe true

  }

  test("add on a non empty set should yield a new Set with two elements") {
    val first: String = randomString
    val second: String = randomString

    first should not be second

    val set: Set[String] = Set(first, second)

    set(first) shouldBe true
    set(second) shouldBe true
  }

  test("remove an element from an empty set should return an empty set") {
    val emptySet: Set[String] = Set.empty[String]
    emptySet(randomString) shouldBe false
  }

  test(
    "remove an element from a non-empty set will return set minus the given element to remove"
  ) {
      val element1: String = randomString
      val element2: String = randomString
      val element3: String = randomString

      val set: Set[String] = Set(element1, element2, element3)

      set(element1) shouldBe true
      set(element2) shouldBe true

      val removeSet: Set[String] = set.remove(element2).remove(element3)

      removeSet(element1) shouldBe true
      removeSet(element2) shouldBe false
      removeSet(element3) shouldBe false
    }

  test("union of an empty set gives an empty set back") {
    val emptySet: Set[String] = Set.empty[String]
    emptySet.union(Set.empty[String])(randomString) shouldBe false

    val union = emptySet.union(emptySet)

    emptySet.isSubSetOf(union) shouldBe true
    union.isSubSetOf(emptySet) shouldBe true

    // Shorter version is below after adding equals override does same as above
    Set.empty[String].union(Set.empty[String]) shouldBe Set.empty[String]
  }

  ignore("just to test union") {
    val aSet: Set[String] = Set("a", "b")

    val aSet1: Set[String] = Set("c", "d")

    aSet1.union(aSet)
  }

  test("union of two non empty set gives a total set with both the elements") {
    val element1: String = randomString
    val element2: String = randomString
    val element3: String = randomString
    val element4: String = randomString

    val set1: Set[String] = Set.empty[String]
      .add(element1)
      .add(element2)

    val set2: Set[String] = Set.empty[String]
      .add(element3)
      .add(element4)

    val unionSet1: Set[String] = set1.union(set2)

    unionSet1(element1) shouldBe true
    unionSet1(element2) shouldBe true
    unionSet1(element3) shouldBe true
    unionSet1(element4) shouldBe true

    val unionSet2: Set[String] = set2.union(set1)

    unionSet2(element1) shouldBe true
    unionSet2(element2) shouldBe true
    unionSet2(element3) shouldBe true
    unionSet2(element4) shouldBe true

    //Shorter version is below after adding equals override does same as above

    set1.union(set2) shouldBe Set(element1, element2, element3).add(element4)
    //OR

    //set1.union(set2) shouldBe set2.union(set1)

  }

  test("union with variance") {
    val (employee, consultant) = bothRoles

    Set[Employee](employee).union(Set[Consultant](consultant)) shouldBe
      Set[CompanyRole](employee, consultant)

    Set[Employee](employee).add(consultant: Consultant) shouldBe Set[CompanyRole](employee, consultant)
  }

  test("intersection of two empty sets will give an empty set back") {
    val emptySet: Set[String] = Set.empty[String]
    val intersectionSet: Set[String] = emptySet.intersection(Set.empty[String])

    intersectionSet(randomString) shouldBe false

    // can also be written as in a better way

    Set.empty.intersection(Set.empty[String]) shouldBe Set.empty

    Set.empty[Nothing].intersection(_ => false) shouldBe Set.empty[Nothing]

  }

  test("intersection of two non empty sets will give only common part of the two set") {
    val element1: String = randomString
    val element2: String = randomString
    val element3: String = randomString
    val element4: String = randomString

    val set1: Set[String] = Set(element1, element2, element3)

    val set2: Set[String] = Set.empty[String].add(element3).add(element4)

    val intersectionSet1: Set[String] = set1.intersection(set2)

    intersectionSet1(element3) shouldBe true
    intersectionSet1(element1) shouldBe false
    intersectionSet1(element2) shouldBe false
    intersectionSet1(element4) shouldBe false

    val intersectionSet2: Set[String] = set2.intersection(set1)

    intersectionSet2(element3) shouldBe true
    intersectionSet2(element1) shouldBe false
    intersectionSet2(element2) shouldBe false
    intersectionSet2(element4) shouldBe false

    //Shorter version is below after adding equals override does same as above

    set1.intersection(set2) shouldBe Set.empty[String].add(element3)
    set2.intersection(set1) shouldBe Set.empty[String].add(element3)

  }

  test("intersection of a non-empty set with empty set should give an empty set") {
    val element1: String = randomString

    val nonEmptySet: Set[String] = Set.empty[String].add(element1)
    val emptySet: Set[String] = Set.empty[String]
    val nonEmptyIntersectEmpty: Set[String] = nonEmptySet.intersection(emptySet)
    val emptyIntersectNonEmpty: Set[String] = emptySet.intersection(nonEmptySet)

    nonEmptyIntersectEmpty(element1) shouldBe false
    emptyIntersectNonEmpty(element1) shouldBe false
  }

  test("difference of two empty set will give an empty set") {
    val anEmptySet: Set[String] = Set.empty[String]

    anEmptySet.difference(Set.empty[String])(randomString) shouldBe false

    // can also be written as in a better way

    Set.empty.difference(Set.empty[String]) shouldBe Set.empty
  }

  test("difference of two non-empty set should give only left set that is getting difference with right one") {
    val element1: String = randomString
    val element2: String = randomString
    val element3: String = randomString
    val element4: String = randomString

    val set1: Set[String] = Set.empty[String].add(element1).add(element2)

    val set2: Set[String] = Set.empty[String].add(element2).add(element3).add(element4)

    val set1DifferenceSet2: Set[String] = set1.difference(set2)

    set1DifferenceSet2(element1) shouldBe true
    set1DifferenceSet2(element2) shouldBe false
    set1DifferenceSet2(element3) shouldBe false
    set1DifferenceSet2(element4) shouldBe false

    val set2DifferenceSet1: Set[String] = set2.difference(set1)

    set2DifferenceSet1(element1) shouldBe false
    set2DifferenceSet1(element2) shouldBe false
    set2DifferenceSet1(element3) shouldBe true
    set2DifferenceSet1(element4) shouldBe true

    //Shorter version
    set1.difference(set2) shouldBe Set.empty[String].add(element1)
    set2.difference(set1) equals Set.empty[String].add(element3).add(element4)
  }

  test("difference on two sets with different types should yield a set with the common type") {

    val (employee, consultant) = bothRoles

    val employeeSet: Set[Employee] = Set(employee)
    val consultantSet: Set[Consultant] = Set(consultant)

    employeeSet.difference(consultantSet) shouldBe employeeSet
    consultantSet.difference(employeeSet) shouldBe consultantSet
  }

  test("isSubSetOf of an empty set should yield true") {
    Set.empty[String].isSubSetOf(Set.empty[String]) shouldBe true
    Set.empty[String].isSubSetOf(Set.empty[String].add(randomString)) shouldBe true

  }

  test("isSubSetOf on itself should yield true") {
    val set1: Set[String] = Set.empty[String].add(randomString)

    set1.isSubSetOf(set1) shouldBe true
  }

  test("isSubSetOf of smallSet should give true for biggerSet which contains all elements of smallSet") {

    val element1: String = randomString
    val element2: String = randomString
    val element3: String = randomString

    val smallSet: Set[String] = Set.empty[String].add(element1)
    val biggerSet: Set[String] = Set(element1, element2, element3)

    smallSet.isSubSetOf(biggerSet) shouldBe true

    biggerSet.isSubSetOf(smallSet) shouldBe false
  }

  test("isSuperSetOf of biggerSet should give true with smallSet being part of it") {
    val element1: String = randomString
    val element2: String = randomString
    val element3: String = randomString

    val smallSet: Set[String] = Set.empty[String].add(element1)
    val biggerSet: Set[String] = Set(element1, element2, element3)

    biggerSet.isSuperSetOf(smallSet) shouldBe true

  }

  test("equals should be reflexive") {
    def reflexive(x: Any): Unit = {
      x shouldBe x
      x.hashCode shouldBe x.hashCode
    }

    reflexive(Set.empty)
    reflexive(Set(1))
    reflexive(Set(1, 2))
    reflexive(Set(2, 1))
  }

  test("equals should be symmetric") {
    def symmetric(x: Any, y: Any): Unit = {
      x shouldBe y
      y shouldBe x
      x.hashCode shouldBe y.hashCode
      y.hashCode shouldBe x.hashCode
    }

    symmetric(Set.empty, Set.empty)
    symmetric(Set(1), Set(1))
    symmetric(Set(1, 2), Set(2, 1))
    symmetric(Set(1, 2), Set(1, 2))
    symmetric(Set(2, 1), Set(2, 1))
    symmetric(Set(2, 1), Set(1, 2))
  }

  test("equals should be transitive") {
    def transitive(x: Any, y: Any, z: Any): Unit = {
      x shouldBe y
      y shouldBe z
      x shouldBe x
      x.hashCode shouldBe y.hashCode
      y.hashCode shouldBe z.hashCode
      x.hashCode shouldBe z.hashCode
    }

    transitive(Set.empty, Set.empty, Set.empty)
    transitive(Set(1, 2, 3), Set(3, 2, 1), Set(2, 1, 3))
  }

  test("these test should be equal") {
    Set(1) should not be Set(2)
    Set(2) should not be Set(1)

    Set(1) should not be 1
  }

  test("hashcode of an empty set should not be random") {
    Set.empty[String].hashCode shouldBe Set.empty[String].hashCode
  }

  test("hashcode of an non-empty set should not be random") {
    val element1: String = randomString
    Set.empty[String].add(element1).hashCode shouldBe Set.empty[String].add(element1).hashCode
  }

  test("hashcode of an empty set should not be zero") {
    Set.empty[String].hashCode should not be 0
  }

  test("hashcode on a non-empty set should be sum of all the hashcodes and hashcode of empty set ") {

    val first: String = randomString
    val second: String = randomString

    val expectedHashcode: Int = Set.empty[String].hashCode + first.hashCode + second.hashCode

    Set(first, second).hashCode shouldBe expectedHashcode
  }

  test("size of an empty set should be zero") {
    Set.empty[String].size shouldBe 0
  }

  test("size of a non-empty set should be equal to number of elements inside the set") {

    val element1: String = randomString
    val element2: String = randomString
    val element3: String = randomString

    val aSet: Set[String] = Set(element1, element2, element3)

    aSet.size shouldBe 3
  }

  test("size of a non-empty set should be equal to number of unique elements inside the set(no repeated)") {

    val element1: String = randomString

    val aSet: Set[String] = Set.empty[String].add(element1).add(element1)

    aSet.size shouldBe 1
  }

  test("isEmpty on an empty set should give true") {
    Set.empty[String].isEmpty shouldBe true
    Set.empty[String].isNonEmpty shouldBe false
  }

  test("isEmpty on a non empty set should give true") {
    Set.empty[String].add(randomString).size shouldBe 1
    Set.empty[String].add(randomString).isEmpty shouldBe false
    Set.empty[String].add(randomString).isNonEmpty shouldBe true
  }

  test("an empty set is not singleton") {
    Set.empty[String].isSingleton shouldBe false
  }

  test("a non-empty set with only on e element is singleton") {
    Set.empty[String].add(randomString).isSingleton shouldBe true
  }

  test("a non-empty set with more than one element is not singleton") {
    Set.empty[String].add(randomString).add(randomString).isSingleton shouldBe false
  }

  test("sample should yield a random element from the Set") {
    Set.empty[String].sample shouldBe None

    val element1: String = randomString
    Set.empty[String].add(element1).sample shouldBe Some(element1)

    val element2: String = randomString
    Set.empty[String].add(element1).add(element2).sample should contain oneOf (element2, element1)

  }

  test("foreach on an empty set should not apply the function") {
    noException should be thrownBy Set.empty[String].foreach { _ =>
      sys.error("should not be thrown")
    }
  }

  test("foreach on a non-empty set should apply a function") {
    var functionWasApplied: Boolean = false
    Set.empty[String].add(randomString).foreach { _ =>
      functionWasApplied = true
    }

    functionWasApplied shouldBe true
  }

  test("foreach should be able to calculate the size of the given empty set") {
    var size: Int = 0

    val emptySet: Set[String] = Set.empty[String]

    emptySet.foreach(_ => size += 1)

    size shouldBe 0
    size shouldBe emptySet.size
  }

  test("foreach should be able to calculate the size of the given non empty set") {
    var size: Int = 0

    val set: Set[String] = Set.empty[String].add(randomString).add(randomString)

    set.foreach(_ => size += 1)

    size shouldBe 2
    size shouldBe set.size
  }

  test("foreach should be able to calculate the size of the given non empty set with unique elements") {
    var size: Int = 0

    val element1: String = randomString
    val set: Set[String] = Set.empty[String].add(element1).add(element1)

    set.foreach(_ => size += 1)

    size shouldBe 1
    size shouldBe set.size
  }

  test("Set without parameters should not compile") {
    "Set()" shouldNot compile
  }

  test("calling the varargs apply method on the set ocmpaining object should yield a set with all the arguments as element") {
    val element1: String = randomString
    val element2: String = randomString
    val element3: String = randomString

    Set(element1, element2, element3) shouldBe Set(element1, element2, element3)

  }

  // Video 4
  test("foreach should be parameterized in the result of the argument function so that it does not give warnings") {
    Set.empty[String].foreach[Int](_ => 1)
  }

  test("map on an empty Set should not apply the function") {
    noException should be thrownBy Set.empty[String].map(_ => sys.error("should not be thrown"))
  }

  test("map should produce a set") {
    Set("hello", "world").map(_.reverse) shouldBe Set("dlrow", "olleh")
  }

  test("map should be able to produce a set of something else other than string") {
    Set("hello", "planet").map(_.size) shouldBe Set(5, 6)
  }

  test("flatmap of a nested set will result in another set of inner set") {
    val aSet: Set[Set[String]] = Set[Set[String]](Set[String]("hello", "world"), Set[String]("john", "doe"))

    val characters: Set[Char] = Set('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    val numbers: Set[Int] = Set(1, 2, 3, 4, 5, 6, 7, 8)

    val aChessBoard: Set[(Char, Int)] = characters.flatmap { c =>
      numbers.map { n =>
        c -> n
      }
    }

    val anotherBoard = characters.map { c =>

      numbers.map { n =>

        c -> n
      }
    }

    /* var someSet: Set[String] = Set.empty[String]
    aSet.flatmap { value =>

      println(value)
      value.map { value2 =>

        // println(value2)
        someSet = someSet.add(value2)
      }
    } */

    //someSet.foreach(println)

    aChessBoard.size shouldBe 64

  }

  test("Set should be a function") {
    val orderedClassmates = Seq("alice", "bob", "frank")

    orderedClassmates.filter(_ != "alice") shouldBe Seq("bob", "frank")

    val friends: Set[String] = Set("frank", "bob")

    orderedClassmates.filter(friends) shouldBe Seq("bob", "frank")

    orderedClassmates.filter(_ != "alice") shouldBe orderedClassmates.filter(friends)
  }

  test("Empty Set does not contain any element") {
    val anEmptySet: Set[String] = Set.empty[String]

    anEmptySet.contains(randomString) shouldBe false
    anEmptySet.doesNotContain(randomString) shouldBe true
  }

  test("A set containing an element should give true") {
    val aNonEmptySet: Set[String] = Set("1")

    aNonEmptySet.contains("1") shouldBe true
    aNonEmptySet.doesNotContain(randomString) shouldBe true
  }

  test("No element exists in empty set") {

    Set.empty[Nothing].exists(_ => false) shouldBe false
    Set.empty[Nothing].doesNotExists(_ => false) shouldBe true

  }

  test("Exists on an non-empty set should yield true") {

    val aNonEmptySet: Set[String] = Set("1", "2")

    aNonEmptySet.exists(_ == "1") shouldBe true
    aNonEmptySet.doesNotExists(_ == "1") shouldBe false

    val anElement: String = randomString

    Set(anElement).exists(_.size == anElement.size) shouldBe true
    Set(anElement).doesNotExists(_.size == anElement.size) shouldBe false

  }

  test("exists with Variance") {
    val (employee, consultant) = bothRoles
    Set(employee).exists(_ == employee) shouldBe true
    Set[CompanyRole](employee).exists(_ == employee) shouldBe true
    Set[Employee](employee).exists(_ == employee) shouldBe true

    /*
      This gives compilation warning outfront
      Set(employee).exists(_ == consultant) shouldBe false
    */

    Set[Employee](employee).exists((input: Employee) => input == employee) shouldBe true
    Set[Employee](employee).exists((input: CompanyRole) => input == employee) shouldBe true
    Set[CompanyRole](employee).exists((input: CompanyRole) => input == employee) shouldBe true
    "Set[CompanyRole](employee).exists((input: Employee) => input == employee)" shouldNot typeCheck

    Set[Employee](employee).exists(Set[Employee](employee)) shouldBe true
    Set[Employee](employee).exists(Set[CompanyRole](employee)) shouldBe true
    Set[CompanyRole](employee).exists(Set[Employee](employee)) shouldBe true
  }

  test("forAll on an empty set should give false") {
    Set.empty[Nothing].forall(_ => false) shouldBe true
  }

  test("forall scenario for a nonEmptySet should give true") {

    val aNonEmptySet: Set[Int] = Set(2, 4, 6, 8)

    aNonEmptySet.forall(_ % 2 == 0) shouldBe true
    aNonEmptySet.notForAll(_ % 2 == 0) shouldBe false
    aNonEmptySet.notForAll(_ % 3 == 0) shouldBe true
    aNonEmptySet.notForAll(_ % 3 == 0) shouldBe true

  }

  test("ToString on an empty set should yield {}") {
    Set.empty.toString shouldBe "{}"

  }

  test("toString of a Set with one element should yield {oneElement}") {
    val element1: String = randomString

    Set(element1).toString shouldBe s"{$element1}"
  }

  test("ToString on a set with two elements should contain 2 braces, both elements, 2 parens and one comma") {
    val element1: String = randomString
    val element2: String = randomString

    element1 should not be element2
    val set1: Set[String] = Set(element1, element2)

    val actual: String = set1.toString

    actual should include(element1)
    actual should include(element2)
    actual should include("{")
    actual should include("}")
    actual should include(",")

    actual.count(_ == '{') shouldBe 1
    actual.count(_ == '}') shouldBe 1
    actual.count(_ == ',') shouldBe 1

  }

  test("ToString on a set with three elements should contain 2 braces, both elements, 2 parens and 2 comma") {
    val element1: String = randomString
    val element2: String = randomString
    val element3: String = randomString

    element1 should not be element2
    val set1: Set[String] = Set(element1, element2, element3)

    val actual: String = set1.toString

    actual should include(element1)
    actual should include(element2)
    actual should include("{")
    actual should include("}")
    actual should include(",")

    actual.count(_ == '{') shouldBe 1
    actual.count(_ == '}') shouldBe 1
    actual.count(_ == ',') shouldBe 2

  }

  private def bothRoles: (Employee, Consultant) = {
    randomEmployee -> randomConsultant
  }

  private def randomConsultant: Consultant = {
    Consultant(
      id          = randomString,
      companyName = randomString
    )
  }

  private def randomEmployee: Employee = {
    Employee(
      id = randomString
    )
  }

  private def randomString: String = {
    scala.util.Random.alphanumeric.take(5).mkString
  }

}

sealed trait CompanyRole {
  def id: String
  final def roleName: String = getClass.toString
}

final case class Employee(id: String) extends CompanyRole {
  def takeVacation(): Unit = {
    println("Taking a vacation")
  }
}

final case class Consultant(id: String, companyName: String) extends CompanyRole {
  def submitInvoice(): Unit = {
    println("Here is my invoice")
  }
}
