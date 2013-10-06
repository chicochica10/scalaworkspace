package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {

    val s0 = singletonSet(0)
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)

    val t3 = singletonSet(3)
    val t4 = singletonSet(4)
    val t5 = singletonSet(5)
    val t6 = singletonSet(6)

    val s12 = union(s1, s2)
    val s123 = union(s12, s3)
    val s1234 = union(s123, s4)
    val s01234 = union(s0, s1234)

    val t34 = union(t3, t4)
    val t345 = union(t34, t5)
    val t3456 = union(t345, t6)

    val s0123456 = union(s01234, t3456)
    val u7 = singletonSet(7)
    val u8 = singletonSet(8)
    val u9 = singletonSet(9)
    val u10 = singletonSet(10)
    val u78 = union(u7, u8)
    val u910 = union(u9, u10)
    val u78910 = union(u78, u910)

    val s10 = union(s0123456, u78910)
    val s_1 = singletonSet(-1)
    val s_2 = singletonSet(-2); val s_12 = union(s_1, s_2)
    val s_3 = singletonSet(-3); val s_123 = union(s_12, s_3)
    val s_4 = singletonSet(-4); val s_1234 = union(s_123, s_4)
    val s_5 = singletonSet(-5); val s_12345 = union(s_1234, s_5)
    val s_6 = singletonSet(-6); val s_123456 = union(s_12345, s_6)
    val s_7 = singletonSet(-7); val s_1234567 = union(s_123456, s_7)
    val s_8 = singletonSet(-8); val s_12345678 = union(s_1234567, s_8)
    val s_9 = singletonSet(-9); val s_123456789 = union(s_12345678, s_9)
    val s_10 = singletonSet(-10); val s_12345678910 = union(s_123456789, s_10)

    val e_10 = singletonSet(-10);
    val e_9 = singletonSet(-9); val e_109 = union(e_10, e_9)
    val e_8 = singletonSet(-8); val e_1098 = union(e_109, e_8)
    val e_7 = singletonSet(-7); val e_10987 = union(e_1098, e_7)
    val e_6 = singletonSet(-6); val e_109876 = union(e_10987, e_6)
    val e_4 = singletonSet(-4); val e_1098764 = union(e_109876, e_4)
    val e_2 = singletonSet(-2); val e_10987642 = union(e_1098764, e_2)
    val e0 = singletonSet(0);
    val e2 = singletonSet(2); val e02 = union(e0, e2)
    val e4 = singletonSet(4); val e024 = union(e02, e4)
    val e6 = singletonSet(6); val e0246 = union(e024, e6)
    val e7 = singletonSet(7); val e02467 = union(e0246, e7)
    val e8 = singletonSet(8); val e024678 = union(e02467, e7)
    val e9 = singletonSet(9); val e0246789 = union(e024678, e9)
    val e10 = singletonSet(10); val e024678910 = union(e0246789, e10)

    val sm_10_10 = union(s_12345678910, s10)
    val sm10_10_par = union(e_10987642, e024678910)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  ignore("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  ignore("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")

    }
  }
  // my own tests

  test("my own tests: singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("my own tests: union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection returns all elements that are both in 's' and 't'") {
    new TestSets {
      val s = union(s1, s2) //{1,2}
      val t = union(s2, s3) //{2,3}
      assert(contains(intersect(s, t), 2), "Intersect 1") //true
      assert(!contains(intersect(s, t), 3), "Intersect 2") //false
    }
  }

  test("diff returns the difference of the two given sets") {
    new TestSets {

      assert(contains(diff(s1234, t3456), 1), "diff 1") //true
      assert(contains(diff(s1234, t3456), 2), "diff 2") //true
      assert(!contains(diff(s1234, t3456), 3), "diff 3") //false
      assert(!contains(diff(s1234, t3456), 4), "diff 4") //false
      assert(!contains(diff(s1234, t3456), 5), "diff 5") //false
      assert(!contains(diff(s1234, t3456), 6), "diff 6") //false
    }
  }

  test("filter returns the subset of 's' for wich 'p' holds") {
    new TestSets {
      val s = union(s1, s2) //{1,2}
      val t = union(s2, s3) //{2,3}
      assert(contains(filter(s, t), 2), "filter 1") //true
      assert(!contains(filter(s, t), 3), "filter 2") //false
    }
  }

  test("forall") {
    new TestSets {

      assert(!forall(sm_10_10, x => x % 2 == 0), "forall 1") // false
      assert(forall(e0246, x => x % 2 == 0), "forall 2") // true

    }
  }

  test("exits") {

    new TestSets {

      val no_par = union(union(singletonSet(1), singletonSet(3)), singletonSet(5))

      assert(exists(sm_10_10, x => x % 2 == 0), "exits 1") // true
      assert(exists(e0246, x => x % 2 == 0), "exits 2") // true
      assert(!exists(no_par, x => x % 2 == 0), "exits 3") // false
    }
  }

  test("map") {

    var conjunto = union(singletonSet(2), singletonSet(3))
    var dobleconjunto = map(conjunto, x => x * 2)

    assert(contains(dobleconjunto, 4), "map 1")
    assert(contains(dobleconjunto, 6), "map 2")

  }
}
