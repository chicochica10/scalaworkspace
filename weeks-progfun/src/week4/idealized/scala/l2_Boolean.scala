package week4.idealized.scala

abstract class Boolean {
	def ifThenElse[T] (t: =>T, e: => T): T
	
	// solo un ejemplo
//	 def sum[T](f: => T, a: T, b: T): T =
//	    f() 
   
}