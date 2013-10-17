package week5

object l3_list_high_order_functions {
  println("high order functionsr")                //> high order functionsr

  // funcion para multiplicar por un factor una lista
  def scaleList1(xs: List[Double], factor: Double): List[Double] = xs match {
    case Nil => Nil
    case x :: xs1 => x * factor :: scaleList1(xs1, factor)
  }                                               //> scaleList1: (xs: List[Double], factor: Double)List[Double]

  var myList = List(1.0, 2, 3)                    //> myList  : List[Double] = List(1.0, 2.0, 3.0)
  scaleList1(myList, 2)                           //> res0: List[Double] = List(2.0, 4.0, 6.0)

  // vamos a aplicar una funcion cualquiera a una lista (MAP)

  // 	abstract class List[T] {...
  // 		def map[U](f: T => U): List[U] = this match {
  // 			case Nil => this
  // 			case x :: xs => f(x) :: xs.map(f)
  // 		}
  // 	}

  // podemos escribir entonces
  def scaleList2(xs: List[Double], factor: Double) = myList map (x => x * factor)
                                                  //> scaleList2: (xs: List[Double], factor: Double)List[Double]
  scaleList2(myList, 2)                           //> res1: List[Double] = List(2.0, 4.0, 6.0)

  //// MAPS CON EL CUADRADO
  def squareList1(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => y * y :: squareList1(ys)
  }                                               //> squareList1: (xs: List[Int])List[Int]

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x * x)                           //> squareList2: (xs: List[Int])List[Int]

  var myList2 = List(1, 2, 3)                     //> myList2  : List[Int] = List(1, 2, 3)
  squareList1(myList2)                            //> res2: List[Int] = List(1, 4, 9)
  squareList2(myList2)                            //> res3: List[Int] = List(1, 4, 9)

  // FILTERING

  def posElems(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case x :: xs => if (x > 0) x :: posElems(xs) else posElems(xs)
  }                                               //> posElems: (xs: List[Int])List[Int]

  var myList3 = List(-1, -3, -1, -5, 6)           //> myList3  : List[Int] = List(-1, -3, -1, -5, 6)

  posElems(myList3)                               //> res4: List[Int] = List(6)
  // hay una definicion generica de filtro para listas (ver video)

  myList3 filter (x => x > 0)                     //> res5: List[Int] = List(6)
  myList3 filterNot (x => x > 0)                  //> res6: List[Int] = List(-1, -3, -1, -5)
  myList3 partition (x => x > 0)                  //> res7: (List[Int], List[Int]) = (List(6),List(-1, -3, -1, -5))

  myList3 takeWhile (x => x < 0)                  //> res8: List[Int] = List(-1, -3, -1, -5)
  myList3 dropWhile (x => x < 0)                  //> res9: List[Int] = List(6)
  myList3 span (x => x < 0)                       //> res10: (List[Int], List[Int]) = (List(-1, -3, -1, -5),List(6))

  // pack exercice

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    // case x :: xs1 =>
    case x :: _ =>
      val (first, rest) = xs span (y => x == y)
      first :: pack(rest) // first es una lista
  }                                               //> pack: [T](xs: List[T])List[List[T]]

  pack(List("a", "a", "a", "b", "c", "c", "a"))   //> res11: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a
                                                  //| ))

  //res11: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a
  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (xs => (xs.head, xs.length))     //> encode: [T](xs: List[T])List[(T, Int)]

  encode(List("a", "a", "a", "b", "c", "c", "a")) //> res12: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))

  //should give
  // List(("a", 3), ("b", 1), ("c", 2), ("a", 1)).

}