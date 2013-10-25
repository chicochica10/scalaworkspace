package week6

import week6._

object l5_for_expressions_and_hig_order_functions {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //NOTAS DEL EJERCICIO l2_handling_nested_sequences
  
  case class Person (name: String, age: Int)
  val persons = List ( Person("angel",21),  Person ("pepe",13))
                                                  //> persons  : List[week6.l5_for_expressions_and_hig_order_functions.Person] = L
                                                  //| ist(Person(angel,21), Person(pepe,13))
  for (p <- persons  if p.age >20) yield p.name   //> res0: List[String] = List(angel)
  
  // es identico a decir
  
  persons filter (person => person.age > 20) map (person => person.name)
                                                  //> res1: List[String] = List(angel)
  //TRADUCCION de highorder funtions a una expresion for:
  
  def map[T,U](xs:List[T], f:T=>U): List[U] = for (x <-xs) yield f(x)
                                                  //> map: [T, U](xs: List[T], f: T => U)List[U]
                                                  
	def flatMap [T,U](xs:List[T], f: T=> Iterable[U]): List[U] = for (x <-xs; y<-f(x)) yield y
                                                  //> flatMap: [T, U](xs: List[T], f: T => Iterable[U])List[U]
  def filter[T](xs: List[T], p: T => Boolean) : List[T] = for (x<-xs if p(x)) yield x
                                                  //> filter: [T](xs: List[T], p: T => Boolean)List[T]
    
    // traduccion real del compilador de scala:
    //1.- for (x <- e1) yield e2 ===> e1.map (e2) (e: expresion)
    //2.- for (x <- e1 if f; s) yield e2 ===> for (x <- e1.withFilter (x => f); s) yield e    (e: expresion f: filtro, s: generador o filtro)
    //NOTA: withFilter : variante de filter que no produce resultado intermedio y filtra el siguiente map o flatMap
    //3.- for (x <- e1; y <- e2; s) yield e3 ===> e1.flatMap ( x => for (y <- e2; s) yield e3)     (s: expresion que puede estar vacia, pueden ser generadores o filtro)
                                                  
	// NOTAS DEL EJERCICIO l2_handling_nested_sequences
	 // ejemplo de los primos con for y sin for
  
   def isPrime(n: Int): Boolean = (2 until n) forall (d => n%d !=0)
                                                  //> isPrime: (n: Int)Boolean
 
  val n= 7                                        //> n  : Int = 7
  (1 until n) flatMap (i => (1 until i) map (j => (i, j))) filter (pair => isPrime (pair._1 +pair._2))
                                                  //> res2: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,
                                                  //| 2), (4,1), (4,3), (5,2), (6,1), (6,5))
  for {
   i <- 1 until n
   j <- 1 until i
   if (isPrime (i+j))
  } yield (i,j)                                   //> res3: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,
                                                  //| 2), (4,1), (4,3), (5,2), (6,1), (6,5))
 // fin notas ahora traducción mecánica siguiendo los slides:
 
 (1 until n).flatMap (i => (1 until i).withFilter (j => isPrime (i+j)).map (j=> (i,j)))
                                                  //> res4: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,
                                                  //| 2), (4,1), (4,3), (5,2), (6,1), (6,5))
 
 // otra traduccion
  case class Book(title: String, authors: List[String])

  // el poner title =, authors= parece optativo se llama 'name parameters' y a veces es más claro
  val books: List[Book] = List( Book(title = "Introduction to Functional Programming", authors = List("Bird, Richard", "Wadler, Phil")))
                                                  //> books  : List[week6.l5_for_expressions_and_hig_order_functions.Book] = List
                                                  //| (Book(Introduction to Functional Programming,List(Bird, Richard, Wadler, Ph
                                                  //| il)))
 //traducir este for
 for (b <- books; a <- b.authors if a startsWith "Bird") yield b.title
                                                  //> res5: List[String] = List(Introduction to Functional Programming)
 
 books flatMap (b => for (a <- b.authors if a startsWith "Bird") yield b.title)
                                                  //> res6: List[String] = List(Introduction to Functional Programming)
 // traduccion del for interior
 //for (a <- b.authors if a startsWith "Bird") yield b.title
//2.- for (x <- e1 if f; s) yield e2 ===> for (x <- e1.withFilter (x => f); s) yield e    (e: expresion f: filtro, s: generador o filtro)
   //for (a <- b.authors.withFilter (a => startsWith "Bird") yield b.title
 books flatMap (b => for (a <- b.authors.withFilter (a => a.startsWith("Bird"))) yield b.title)
                                                  //> res7: List[String] = List(Introduction to Functional Programming)
// eliminar el for interior
//  for (a <- b.authors.withFilter (a => a.startsWith("Bird"))) yield b.title
 // 1.- for (x <- e1) yield e2 ===> e1.map (e2)
 //for (a <- b.authors.withFilter (a => a.startsWith("Bird"))) yield b.title
 //b.authors.withFilter (a => a.startsWith("Bird")).map (b=>b.title)

 //books flatMap (b => (b.authors.withFilter (a => a.startsWith("Bird")).map (b=> b.title))) NO FUNCIONA


// PARA QUE for FUNCIONE CON NUESTROS PROPIOS TIPOS HAY QUE IMPLEMENTAR map, flatMap y withFilter
}