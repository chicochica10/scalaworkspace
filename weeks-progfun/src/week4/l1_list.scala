package week4


trait List1[T]{
	def isEmpty: Boolean
	def head: T
	def tail: List1[T]
}
class Cons1[T] (val head:T, val tail: List1[T]) extends List1[T]{
	def isEmpty = false //nunca empty
}

class Nil1[T] extends List1[T]{	
	def isEmpty = true //siempre vacia
	def head = throw new NoSuchElementException ("Nil1.head") 
	def tail = throw new NoSuchElementException ("Nil1.tail") 
}
// al implementar apply en el objeto consiguimos crear parámetros para ese objeto!!!

object List1 {
  
  //List1 (1,2) seria una funcion con lo cual tb tiene un metodo apply: (ver l0_funtions_as_objects para mas info)
  //List1 (1,2) = List.apply (1,2)
  def apply[T] (elem1: T, elem2: T) = new Cons1 (elem1, new Cons1 (elem2, new Nil1))
  
  def apply[T] () = new Nil1
  def apply[T] (elem: T) = new Cons1 (elem, new Nil1)
}

object L1_list {
 def main (args: Array[String]) ={
 // println (List1().head)
  println (List1(1).head)
  println (List1(2,3).head + " " + List1(2,3).tail.head )
 }
}

