package week4


trait List2[T]{
	def isEmpty: Boolean
	def head: T
	def tail: List2[T]
}
class Cons1[T] (val head:T, val tail: List2[T]) extends List2[T]{
	def isEmpty = false //nunca empty
}

class Nil1[T] extends List2[T]{	
	def isEmpty = true //siempre vacia
	def head = throw new NoSuchElementException ("Nil1.head") 
	def tail = throw new NoSuchElementException ("Nil1.tail") 
}
// al implementar apply en el objeto consiguimos crear parámetros para ese objeto!!!

object List2 {
  
  //List2 (1,2) seria una funcion con lo cual tb tiene un metodo apply: (ver l0_funtions_as_objects para mas info)
  //List2 (1,2) = List2.apply (1,2)
  def apply[T] (elem1: T, elem2: T) = new Cons1 (elem1, new Cons1 (elem2, new Nil1))
  
  def apply[T] () = new Nil1
  def apply[T] (elem: T) = new Cons1 (elem, new Nil1)
}

object L1_List2 {
 def main (args: Array[String]) ={
   println (List2)
 // println (List2().head)
  println (List2(1).head)
  println (List2(2,3).head + " " + List2(2,3).tail.head )
 }
}

