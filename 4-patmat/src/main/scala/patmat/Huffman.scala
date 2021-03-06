package patmat

import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match {
    case Fork(left, right, chars, weight) => weight
    case Leaf(char, weight) => weight

  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(left, right, chars, weight) => chars
    case Leaf(char, weight) => List(char)
  }

  //:::(prefix: List[A]): List[A]
  //[use case] Adds the elements of a given list in front of this list.
  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // ejemplo
  object Test {
    val sampleTree = makeCodeTree(
      makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
      Leaf('t', 2))
  }

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  //  def times_old(chars: List[Char]): List[(Char, Int)] = chars match {
  //    case Nil => Nil
  //    case c :: cs =>
  //      {
  //        // si c esta en el listado de (char,int) deovlver la lista nueva
  //        // con la tupla incrementada
  //        var tupla = (c, _)
  //        var index = times_old(cs).indexOf(tupla)
  //        if (index > 0) { // devuleve una lista de tuplas
  //          var elem = times_old(cs).apply(index)
  //          var inc = elem._2 + 1
  //          times_old(cs).updated(index, (c, inc))
  //        } // si no crear una nueva lista con los elementos que hubiera y 
  //        // meter en esa lista una tupla con (c,1)  
  //        else {
  //          (c, 1) :: times_old(cs)
  //        }
  //      }
  //  }
  //  
  def times(chars: List[Char]): List[(Char, Int)] = chars match {
    case Nil => Nil
    case c :: cs =>
      {
        val tuplas = times(cs)
        val myIndex = index(c, tuplas)
        if (myIndex >= 0) {
          tuplas.updated(myIndex, (c, tuplas.apply(myIndex)._2 + 1))
        } else {
          tuplas :+ (c, 1)
        }
      }
  }
  def index(c: Char, tuplas: List[(Char, Int)]): Int = indexAux(c, tuplas, 0)

  def indexAux(c: Char, tuplas: List[(Char, Int)], acc: Int): Int = tuplas match {
    case Nil => -1
    case ((x, _) :: xs) => if (c == x) acc
    else indexAux(c, xs, acc + 1)
  }

  // implementación alternativa
  //  def times(chars: List[Char]): List[(Char, Int)] = {
  //		def occurs(char: Char, count: List[(Char,Int)]): List[(Char, Int)] = {
  //			if( count.isEmpty )
  //				count :+ (char, 1)
  //			else count.head match {
  //				case (ch, num) => 
  //					if( ch == char ) (char, num+1) :: count.tail
  //					else count.head :: occurs(char, count.tail)
  //			}
  //		}
  //		if( chars.isEmpty )
  //			List[(Char, Int)]()
  //		else
  //			occurs(chars.head, times(chars.tail))
  //	}

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs match {
    case Nil => Nil
    case (c, f) :: cfx => insertOrdered((c, f), makeOrderedLeafList(cfx))
  }

  def insertOrdered(x: (Char, Int), xs: List[Leaf]): List[Leaf] = xs match {
    case Nil => List(new Leaf(x._1, x._2))
    case y :: ys => if (x._2 > y.weight) y :: insertOrdered(x, ys) else new Leaf(x._1, x._2) :: xs
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */

  //  Write a function combine which (1) removes the two trees with the lowest weight from
  //  the list constructed in the previous step, and (2) merges them by creating a new node of type Fork.
  //  Add this new tree to the list - which is now one element shorter - while preserving the order (by weight).

    def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
      case Nil => trees
      case x :: Nil => trees
      case x :: y :: Nil => if (weight(x) < weight(y)) x :: y :: Nil else y :: x :: Nil
      case x :: y :: xs => {
        val comb = new Fork(x, y, chars(x) ::: chars(y), weight(x) + weight(y))
        //combine(comb :: xs).sortBy(ct => weight(ct))
        combine(comb :: xs).sortWith((a, b) => weight(a) < weight(b))
  
      }
    }

//  def combine(trees: List[CodeTree]): List[CodeTree] = {
//    if (trees.isEmpty) List[CodeTree]()
//    else if (trees.tail.isEmpty) trees
//    else {
//      val t1 = trees.head
//      val t2 = trees.tail.head
//
//      val combined = Fork(t1, t2, chars(t1) ::: chars(t2), weight(t1) + weight(t2))
//
//      (combined :: trees.tail.tail).sortWith((a, b) => weight(a) < weight(b))
//    }
//  }
  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */

  //Write a function until which calls the two functions defined above until this list contains only a single tree. 
  //This tree is the optimal coding tree. The function until can be used in the following way:
  //until(singleton, combine)(trees)
  //where the argument trees is of the type List[CodeTree].

  //def until (xxx: ???, yyy: ???)(zzz: ???): ??? = ???
  //  def until(isSingleton: List[CodeTree] => Boolean,
  //    combined: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): CodeTree = {
  //
  //    if (isSingleton(trees)) {
  //      trees.head
  //    } else {
  //      until(isSingleton, combined)(combined(trees.tail))
  //    }
  //  }

  //mejorada
  def until[T](isSingleton: List[T] => Boolean,
    combined: List[T] => List[T])(trees: List[T]): T = {

    if (isSingleton(trees)) {
      trees.head
    } else {
      until(isSingleton, combined)(combined(trees.tail))
    }
  }
  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  //Finally, use the functions defined above to implement the function createCodeTree which respects the signature shown above.
  def createCodeTree(chars: List[Char]): CodeTree = {

    until(singleton, combine)(makeOrderedLeafList(times(chars)))
  }

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  //Decoding also starts at the root of the tree. Given a sequence of bits to decode, we successively read the bits, 
  //and for each 0, we choose the left branch, and for each 1 we choose the right branch.
  //When we reach a leaf, we decode the corresponding character and then start again at the root of the tree. 
  //As an example, given the Huffman tree above, the sequence of bits, 10001010 corresponds to BAC.

  //abstract class CodeTree
  //case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  //case class Leaf(char: Char, weight: Int) extends CodeTree

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {

    def decodeAux(subtree: CodeTree, bits: List[Bit]): List[Char] = subtree match {
      case Leaf(c, _) => c :: decodeAux(tree, bits) //c :: Nil no sirve pq tenemos que recorrer toda la lista de bits y hay que llamar a decode de alguna manera con la lista de bits que queden
      //                                                   NOTA. se hace referencia al tree y no al subtree el decir tenemos que ir desde el principio al raiz con los bits que queden segun la definicion
      case Fork(left, right, _, _) => if (bits.isEmpty) Nil // hay que proteger de acceder a una lista nulan
      else if (bits.head == 0) {
        decodeAux(left, bits.tail)
      } else {
        decodeAux(right, bits.tail)
      }
    }
    decodeAux(tree, bits)
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  //For a given Huffman tree, one can obtain the encoded representation of a character by traversing from the root of the tree to the leaf 
  //containing the character. Along the way, when a left branch is chosen, a 0 is added to the representation, 
  //and when a right branch is chosen, 1 is added to the representation. 
  //Thus, for the Huffman tree above, the character D is encoded as 1011.

  // Your implementation must traverse the coding tree for each character, a task that should be done using a helper function.

  //abstract class CodeTree
  //case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  //case class Leaf(char: Char, weight: Int) extends CodeTree

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeAux(subtree: CodeTree)(text: List[Char]): List[Bit] = {
      if (text.isEmpty) Nil
      else {
        subtree match {
          case Leaf(_, _) => encodeAux(tree)(text.tail)
          case Fork(left, right, _, _) => if (chars(left).contains(text.head)) 0 :: encodeAux(left)(text)
          else 1 :: encodeAux(right)(text)
        }
      }
    }

    encodeAux(tree)(text)
  }

  //  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  //    def encodeAux(subtree: CodeTree)(text: List[Char]): List[Bit] = subtree match {
  //
  //      case Leaf(_, _) => encodeAux(tree)(text.tail) // falta la comprobacion de nil en el text
  //      case Fork(left, right, cs, _) => cs match { // no sirve pq cs siempre va a tener una lista
  //        case Nil => Nil
  //        case x :: xs => if (chars(left).contains(x)) 0 :: encode(subtree)(text)
  //        else 1 :: encode(subtree)(text)
  //      }
  //    }
  //
  //    encodeAux(tree)(text)
  //  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
    case Nil => Nil
    case (x, lb) :: xs => if (x == char) lb else codeBits(xs)(char)
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  // def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {

  def convert(tree: CodeTree): CodeTable = {

    def convertAux(subTree: CodeTree): CodeTable = subTree match {
      case Leaf(c, _) => (c, encode(tree)(c :: Nil)) :: Nil //ojo encoding de todo el trozo de arbol hasta la hoja
      case Fork(left, right, _, _) => convertAux(left) ::: convertAux(right)
    }
    convertAux(tree)
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val codeTable = convert(tree)
    def quickEncodeAux(text: List[Char]): List[Bit] = text match {
      case Nil => Nil
      case c :: xs => codeBits(codeTable)(c) ::: quickEncodeAux(xs)
    }

    quickEncodeAux(text)
  }
}
