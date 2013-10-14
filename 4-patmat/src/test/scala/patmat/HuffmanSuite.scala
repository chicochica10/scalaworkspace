package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("index") {

    var l = List(('a', 1), ('b', 2), ('c', 3), ('d', 4))
    assert(index('a', l) == 0)
    assert(index('b', l) == 1)
    assert(index('c', l) == 2)
    assert(index('d', l) == 3)
    assert(index('e', l) == -1)
    assert(index('a', List()) == -1)

  }

  test("times") {
    var l = times(List('a', 'b', 'a'))
    println("items: " + l.length)
    println("0._1-> " + l.apply(0)._1 + " 0._2-> " + l.apply(0)._2)
    println("1._1-> " + l.apply(1)._1 + " 1._2-> " + l.apply(1)._2)
    //  println ("2._1-> " + l.apply(2)._1 + " 2._2-> " + l.apply(2)._2)

    assert(times(List('a', 'b', 'a')) == List(('a', 2), ('b', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
    assert(makeOrderedLeafList(List(('t', 3), ('e', 1), ('x', 2))) === List(Leaf('e', 1), Leaf('x', 2), Leaf('t', 3)))
    assert(makeOrderedLeafList(List(('t', 3), ('e', 2), ('x', 1))) === List(Leaf('x', 1), Leaf('e', 2), Leaf('t', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
    val leaflist2 = List(Leaf('e', 1), Leaf('t', 4), Leaf('x', 4))
    assert(combine(leaflist2) === List(Leaf('x', 4), Fork(Leaf('e', 1), Leaf('t', 4), List('e', 't'), 5)))
  }

  test("descode french") {
    def b = new StringBuilder()
    def d = decodedSecret.addString(b)
    println("french -> " + d.toString())
  }
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
