package homegrown.collections

import homegrown.collections._
/* import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers */

class SetSuiteOld /* extends AnyFunSuite with Matchers */ {

  /*test("apply on empty set should yield false") {
    OldSet.empty(randomString) shouldBe false

  }

  test("Add on a empty set should yield a new set with one element") {
    val first: String = randomString
    val second: String = randomString

    first should not be second

    val set = OldSet.empty.add(first)
    set(first) shouldBe true

    set(second) shouldBe false

  }

  test("Add on a non-empty should yield a new set with two elements") {

    val first: String = randomString
    val second: String = randomString

    first should not be second

    val set = OldSet.empty
      .add(first)
    //.add(second)

    val secondOldSet = set.add(second)

    //set(first) shouldBe true
    secondOldSet(second) shouldBe true
    secondOldSet(first) shouldBe true

  }

  test("Delete an element from a set should be successful") {
    val element: String = randomString
    val aOldSet: OldSet = OldSet.empty.add(element)

    val aOldSetWithoutTheElement = aOldSet.remove(element)

    aOldSet(element) shouldBe true
    aOldSetWithoutTheElement(element) shouldBe false
  }*/

  // test("Remove on an empty set should yield an empty set") {
  //   val element: String = randomString
  //   val emptyOldSet: OldSet = OldSet.empty.remove(element)

  //   emptyOldSet(element) shouldBe false
  // }

  // test(
  //   "Removing an element from a set containing more than 2 distinct elements should return element without given element"
  // ) {
  //     val element: String = randomString
  //     val element2: String = randomString
  //     val element3: String = randomString
  //     val element4: String = randomString

  //     val aOldSet = OldSet.empty.add(element).add(element2).add(element3)

  //     val tt = OldSet.empty.add(element)

  //     val tt1: OldSet = tt.add(element2)
  //     val tt2: OldSet = tt1.add(element3)
  //     val tt3: OldSet = tt2.add(element4)

  //     //tt(element)

  //     //tt3(element)
  //     tt3(element) shouldBe true
  //     tt3(element2) shouldBe true
  //     tt3(element3) shouldBe true
  //     tt3(element2) shouldBe true

  //     val aOldSetWithoutElement: OldSet = aOldSet.remove(element2)

  //     //println(aOldSetWithoutElement)

  //     //aOldSet(element) shouldBe true
  //     //aOldSet(element2) shouldBe true

  //     aOldSetWithoutElement(v1 = element) shouldBe true
  //     aOldSetWithoutElement(element2) shouldBe false
  //     aOldSetWithoutElement(element3) shouldBe true

  //   }

  // test("Union on empty should give an empty set") {
  //   OldSet.empty.union(OldSet.empty)(randomString) shouldBe false
  // }

  // test("Union of non-empty with union set should give orignal set") {
  //   val element1: String = randomString
  //   val element2: String = randomString

  //   element1 should not be element2

  //   val emptyOldSet: OldSet = OldSet.empty
  //   val nonEmptyOldSet: OldSet = emptyOldSet.add(element1).add(element2)

  //   emptyOldSet.union(nonEmptyOldSet)(element1) shouldBe true
  //   emptyOldSet.union(nonEmptyOldSet)(element2) shouldBe true

  //   nonEmptyOldSet.union(emptyOldSet)(element2) shouldBe true
  //   nonEmptyOldSet.union(emptyOldSet)(element2) shouldBe true
  // }

  // test("Union of two non-empty set should give a big set of combined set") {

  //   val element1: String = randomString
  //   val element2: String = randomString
  //   val element3: String = randomString
  //   val element4: String = randomString

  //   val set1: OldSet = OldSet.empty.add(element1).add(element2)
  //   val set2: OldSet = OldSet.empty.add(element3).add(element4)

  //   set1.union(set2)(element1) shouldBe true
  //   set1.union(set2)(element2) shouldBe true
  //   set1.union(set2)(element3) shouldBe true
  //   set1.union(set2)(element4) shouldBe true

  //   set2.union(set1)(element1) shouldBe true
  //   set2.union(set1)(element2) shouldBe true
  //   set2.union(set1)(element3) shouldBe true
  //   set2.union(set1)(element4) shouldBe true

  // }

  // test("Intersection of an empty set with non empty set should give empty set") {
  //   val element1: String = randomString

  //   val emptyOldSet: OldSet = OldSet.empty
  //   val nonEmptyOldSet: OldSet = OldSet.empty.add(element1)

  //   emptyOldSet.intersection(nonEmptyOldSet)(element1) shouldBe false

  //   nonEmptyOldSet.intersection(emptyOldSet)(element1) shouldBe false

  // }

  // test("Intersection between two non empty set should give common element") {

  //   val element1: String = randomString
  //   val element2: String = randomString
  //   val element3: String = randomString

  //   val set1: OldSet = OldSet.empty.add(element1).add(element2)
  //   val set2: OldSet = OldSet.empty.add(element2).add(element3)

  //   set1.intersection(set2)(element2) shouldBe true
  //   set1.intersection(set2)(element1) shouldBe false
  //   set1.intersection(set2)(element3) shouldBe false

  //   set2.intersection(set1)(element2) shouldBe true
  //   set2.intersection(set1)(element1) shouldBe false
  //   set2.intersection(set1)(element3) shouldBe false

  // }

  // test("Difference between two non-empty set should give only the element from left set plus common element from right") {
  //   val element1: String = randomString
  //   val element2: String = randomString
  //   val element3: String = randomString
  //   val element4: String = randomString

  //   val set1: OldSet = OldSet.empty.add(element1).add(element2).add(element3)

  //   val set2: OldSet = OldSet.empty.add(element3).add(element4)

  //   val difference1 = set1.difference(set2)

  //   difference1(element1) shouldBe true
  //   difference1(element2) shouldBe true
  //   difference1(element3) shouldBe false
  //   difference1(element4) shouldBe false

  //   val difference2: OldSet = set2.difference(set1)
  //   difference2(element4) shouldBe true
  //   difference2(element3) shouldBe false
  //   difference2(element1) shouldBe false
  //   difference2(element2) shouldBe false

  // }

  // test("isSubOldSetOf on an empty OldSet should yield true") {
  //   pending
  //   OldSet.empty.isSubOldSetOf(OldSet.empty) shouldBe true
  //   OldSet.empty.isSubOldSetOf(OldSet.empty.add(randomString)) shouldBe true

  // }

  // private def randomString: String = {
  //   scala.util.Random.alphanumeric.take(5).mkString
  // }

}
