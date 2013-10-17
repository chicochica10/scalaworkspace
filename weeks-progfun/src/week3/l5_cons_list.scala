package week3



//trait IntList //...

//NOTA: val en el constructor define el parametro head y crea la propiedad o campo head de la clase
//class Cons (val head:Int, val tail: IntList) extends IntList //... NOTA: el trait tambien se puede EXTENDER no solo implementar

//class Nil extends IntList

//^ la forma de definirlo arriba no escala pq si queremos contruir una lista de por ejemplo booleanos tendriamos que definirla aparte para ello usamos Type Parameters:

trait List[T]{
	def isEmpty: Boolean
	def head: T
	def tail: List[T]
}


class Cons[T] (val head:T, val tail: List[T]) extends List[T]{
	def isEmpty = false //nunca empty
	//el head al tener val el parametro no hace falta ya es un campo IDEM Para tail
	//NOTA DIFERENCIA ENTRE val y def..... val es evaluado cuando se inicializa por primera vez y def es evaluado cada vez que se le referencia
}

class Nil[T] extends List[T]{
	//def isEmpty: Boolean = true //siempre vacia
	//def head: Nothing = throw new NoSuchElementException ("Nil.head") //el tipo de vuelta es Nothing
	//def tail: Nothing = throw new NoSuchElementException ("Nil.tail") //el tipo de vuelta es Nothing

	def isEmpty = true //siempre vacia
	def head = throw new NoSuchElementException ("Nil.head") 
	def tail = throw new NoSuchElementException ("Nil.tail") 

}

//podemos crear tb funciones con Type Parameters
// esta crea una lista con un solo elemento
object myLista{
	def singleton[T] (elem: T) = new Cons (elem, new Nil[T])

	singleton[Int](1) //los type parameters son redundantes
	singleton[Boolean](true) //los type parameters son redundantes

	def enteros = singleton(1)
	def boleanos = singleton(true)

}