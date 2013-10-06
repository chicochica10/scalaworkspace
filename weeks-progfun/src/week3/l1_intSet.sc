package week3

object l1_intSet {
  println("l1_intSet")                            //> l1_intSet
  // new IntSet es una clase abastracta
  //val t1 = new NonEmpty (3, new Empty, new Empty) no hace falta hacer el new de Empty
  val t1 = new NonEmpty (3, Empty, Empty)         //> t1  : week3.NonEmpty = {.3.}
  val t2 = t1 incl 4                              //> t2  : week3.IntSet = {.3{.4.}}
  
  
  
}

//superclass
abstract class IntSet { //si se quita abastract hay que añadir los miembros
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  def union (other: IntSet) : IntSet
}

//implementacion usuando binary trees
//class Empty extends IntSet {
//  def contains(x: Int): Boolean = false
//  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
//  override def toString = "." //significa vacio
//}

//MEJORA.... CON object creamos un SINGLETON que siempre es el mismo no other Empty instances can be or need to be created
object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x,  Empty,  Empty) // no hace falta hacer el new
  def union (other: IntSet) = other
  override def toString = "." //significa vacio
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x //left.contains (x)
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x /*left.incl(x)*/ , right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this //x == elem
    
  def union (other: IntSet): IntSet =
  	((left union right) union other) incl elem
   
  override def toString = "{" + left + elem + right +"}"
}