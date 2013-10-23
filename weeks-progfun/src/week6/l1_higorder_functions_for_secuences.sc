package week6

object l1_conversion_array_vector {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val xs = Array (1,3,44)                         //> xs  : Array[Int] = Array(1, 3, 44)
	xs map (x => x *2)                        //> res0: Array[Int] = Array(2, 6, 88)
	
	val st = "Hello wOrld"                    //> st  : String = Hello wOrld
	st filter (c => c.isUpper)                //> res1: String = HO
	
	val r: Range = 1 until 5                  //> r  : Range = Range(1, 2, 3, 4)
	val s: Range = 1 to 5                     //> s  : Range = Range(1, 2, 3, 4, 5)
	1 to 10 by 3                              //> res2: scala.collection.immutable.Range = Range(1, 4, 7, 10)
	6 to 1 by -2                              //> res3: scala.collection.immutable.Range = Range(6, 4, 2)
	
	st exists (c => c.isUpper)                //> res4: Boolean = true
	st forall (c => c.isUpper)                //> res5: Boolean = false
	
	val pair = List (1,2,3) zip st            //> pair  : List[(Int, Char)] = List((1,H), (2,e), (3,l))
	pair unzip                                //> res6: (List[Int], List[Char]) = (List(1, 2, 3),List(H, e, l))
	
	st flatMap (c=>List('.',c))  // aplica una funcion que devuelve una coleccion y va concanteando los elementos de esa coleccion una vez aplicada
                                                  //> res7: String = .H.e.l.l.o. .w.O.r.l.d
	
	xs.sum                                    //> res8: Int = 48
	xs.max                                    //> res9: Int = 44
	
	
	// el mas jodido
	// posibles combinaciones de numeros entre 1..M y 1..N
	(1 to 5) flatMap (x=> (1 to 3) map (y => (x,y)))
                                                  //> res10: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (1,
                                                  //| 2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3), (4,1), (4,2), (4,3), (5
                                                  //| ,1), (5,2), (5,3))
	// multiplicacion de dos vectores
	def scalarProduct (v1: Vector[Double], v2: Vector[Double]): Double = {
	 ((v1 zip v2) map (xy => xy._1 * xy._2)).sum
	 
	}                                         //> scalarProduct: (v1: Vector[Double], v2: Vector[Double])Double
	
	val sp = scalarProduct(Vector(1,1,1), Vector (1,2,1))
                                                  //> sp  : Double = 4.0
  // usando patter matching
  
  def scalarProduct1 (v1: Vector [Double], v2: Vector[Double]): Double =
  (v1 zip v2).map { case (x, y) => x*y }.sum      //> scalarProduct1: (v1: Vector[Double], v2: Vector[Double])Double
  
  
  val sp1 = scalarProduct1(Vector(1,1,1), Vector (1,2,1))
                                                  //> sp1  : Double = 4.0
 // porque es equivalente a :
 
 def scalarProduct2 (v1: Vector [Double], v2: Vector[Double]): Double =
  (v1 zip v2).map ( x => x match{ case (x, y) => x*y }).sum
                                                  //> scalarProduct2: (v1: Vector[Double], v2: Vector[Double])Double
  
  
  val sp2 = scalarProduct2(Vector(1,1,1), Vector (1,2,1))
                                                  //> sp2  : Double = 4.0
                                                  
// para todos los elementos desde 2 hasta n (no includido) su division no tiene que ser exacta
 def isPrime(n: Int): Boolean = (2 until n) forall (d => n%d !=0)
                                                  //> isPrime: (n: Int)Boolean
 
 isPrime (5)                                      //> res11: Boolean = true
 isPrime (6)                                      //> res12: Boolean = false
 }
 
 