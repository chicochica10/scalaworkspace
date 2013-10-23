package week6

object l2_handling_nested_sequences {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  // dado un entero positivo n, encontrar pares (i,j)
  // tal que 1 <= j < i < n y que la suma de i + j sea un primo
  // ver slides
  // 1.- calcular los pares hasta n
  val n = 7                                       //> n  : Int = 7
  val xss =(1 until n) map (i => (1 until i) map (j => (i, j)))
                                                  //> xss  : scala.collection.immutable.IndexedSeq[scala.collection.immutable.Inde
                                                  //| xedSeq[(Int, Int)]] = Vector(Vector(), Vector((2,1)), Vector((3,1), (3,2)), 
                                                  //| Vector((4,1), (4,2), (4,3)), Vector((5,1), (5,2), (5,3), (5,4)), Vector((6,1
                                                  //| ), (6,2), (6,3), (6,4), (6,5)))
  // tenemos un vector de vectores no una lista de pares
  // son vectores pq los pares no se pueden hacer con rangos asi que se sube arriba en la jerarquia de rango y el tipo superior en
  //IndexedSecuence y la implementacion por defecto de indexsecuence es vector
  // vamos a conseguir una lista de pares
  // se puede hacer con
  //def
  //foldRight[B](z: B)(op: (A, B) ⇒ B): B
  //Applies a binary operator to all elements of this vector and a start value, going right to left.
  // o con
  //def
  //flatten[B]: Vector[B]
  //[use case] Converts this vector of traversable collections into a vector formed by the elements of these traversable collections.
  //(xss foldRight Seq[Int]())(_ ++ _) ????? NO FUNCIONA
  
  xss.flatten                                     //> res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,
                                                  //| 1), (3,2), (4,1), (4,2), (4,3), (5,1), (5,2), (5,3), (5,4), (6,1), (6,2), (
                                                  //| 6,3), (6,4), (6,5))
  // regla util ver ejercercicio jodido de l1_highorder_functions_for_secuences
  // flaten map
  //=========================================
  // xs flatMap f = (xs map f).flatten
  //=========================================
  // por lo que podemos escribir
  
  (1 until n) flatMap (i => (1 until i) map (j => (i, j)))
                                                  //> res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,
                                                  //| 1), (3,2), (4,1), (4,2), (4,3), (5,1), (5,2), (5,3), (5,4), (6,1), (6,2), (
                                                  //| 6,3), (6,4), (6,5))
  // para obtener los primos
  def isPrime(n: Int): Boolean = (2 until n) forall (d => n%d !=0)
                                                  //> isPrime: (n: Int)Boolean
 
  
  (1 until n) flatMap (i => (1 until i) map (j => (i, j))) filter (pair => isPrime (pair._1 +pair._2))
                                                  //> res2: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,
                                                  //| 2), (4,1), (4,3), (5,2), (6,1), (6,5))
  // como esto es jodido de entender metemos las expresiones for
  
  //lleva el case OJO!!!
  case class Person (name: String, age: Int)
  val persons = List (new Person("angel",21), new Person ("pepe",13))
                                                  //> persons  : List[week6.l2_handling_nested_sequences.Person] = List(Person(an
                                                  //| gel,21), Person(pepe,13))
  // se pueden poner {}
  for (p <- persons  if p.age >20) yield p.name   //> res3: List[String] = List(angel)
  
  // es identico a decir
  
  persons filter (person => person.age > 20) map (person => person.name)
                                                  //> res4: List[String] = List(angel)
                                                  
  // ejemplo de los primos con for
  for {
   i <- 1 until n
   j <- 1 until i
   if (isPrime (i+j))
  } yield (i,j)                                   //> res5: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,
                                                  //| 2), (4,1), (4,3), (5,2), (6,1), (6,5))
  
  //producto escalar viejo
    def scalarProduct1 (v1: Vector [Double], v2: Vector[Double]): Double =
  (v1 zip v2).map { case (x, y) => x*y }.sum      //> scalarProduct1: (v1: Vector[Double], v2: Vector[Double])Double
  
  // producto escalar con for
  def scalarProduct(xs: List[Double], ys: List[Double]) : Double =
  (for { (x,y) <- xs zip ys } yield x*y).sum      //> scalarProduct: (xs: List[Double], ys: List[Double])Double
  
  scalarProduct (List(1,1,1), List(1,1,1))        //> res6: Double = 3.0
}