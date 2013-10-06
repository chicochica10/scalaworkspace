package week3

object l2_override {
  println("override")                             //> override
}


// redefinicion de metodos en subclases
abstract class Base {
	def foo = 1 //ojo no está implementado pero tiene un valor pq es una property
	def bar: Int
}
class Sub extends Base {
	override def foo = 2
	def bar = 3
}