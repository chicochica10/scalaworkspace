package week5

object l2_merge_sort {
  println("merge sort")                           //> merge sort
  //Mi idea
  def msort0(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val l = xs take n
      val r = xs drop n
      def merge(ls: List[Int], rs: List[Int]) = ???
      merge(msort0(l), msort0(r))
    }
  }                                               //> msort0: (xs: List[Int])List[Int]

  // con split
  def msort1(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]) = ???
      val (fst, snd) = xs splitAt n
      merge(msort1(fst), msort1(snd))
    }
  }                                               //> msort1: (xs: List[Int])List[Int]

  // merge implementation
  def msort2(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
        case Nil => ys
        case x :: xs1 => ys match {
          case Nil => xs
          // no funciona case y :: ys1 => if (x < y) x :: y :: merge(xs1,ys1) else y :: x :: merge(xs1,ys1) // pq avanzo en las dos listas
          case y :: ys1 => if (x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
        }
      }

      val (fst, snd) = xs splitAt n
      merge(msort2(fst), msort2(snd))
    }
  }                                               //> msort2: (xs: List[Int])List[Int]

  val myList = List(0, 7, 5, 1, 4, 3, 6, 2, 8, 9) //> myList  : List[Int] = List(0, 7, 5, 1, 4, 3, 6, 2, 8, 9)
  msort2(myList)                                  //> res0: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

  // merge implementation with pairs
  def msort3(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] =
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) => if (x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
        }

      val (fst, snd) = xs splitAt n
      merge(msort3(fst), msort3(snd))
    }
  }                                               //> msort3: (xs: List[Int])List[Int]

  msort3(myList)                                  //> res1: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

  //merge Implementation with generics
  //añadimos como parametro la funcion de comparacion pero en msort no en merge
  def msort4[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      //ojo antes tenia puesto  def merge[T](xs: List[T], ys: List[T]): List[T] = y se estaban machacando las T
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          // problema no sabemos hacer la comparacion
          // case (x::xs1, y::ys1) => if (x < y)  x :: merge(xs1,ys) else y :: merge(xs,ys1)
          case (x :: xs1, y :: ys1) => if (lt(x, y))
            x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
        }

      val (fst, snd) = xs splitAt n
      merge(msort4(fst)(lt), msort4(snd)(lt))
    }
  }                                               //> msort4: [T](xs: List[T])(lt: (T, T) => Boolean)List[T]

  msort4(myList)((x: Int, y: Int) => x < y)       //> res2: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

  var fruits = List("apple", "pear", "banana")    //> fruits  : List[String] = List(apple, pear, banana)
  msort4(fruits)((x: String, y: String) => x.compare(y) < 0)
                                                  //> res3: List[String] = List(apple, banana, pear)

  //los tipos no son necesarios el compilador de scala los infiere
  msort4(myList)((x, y) => x < y)                 //> res4: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  msort4(fruits)((x, y) => x.compare(y) < 0)      //> res5: List[String] = List(apple, banana, pear)

  // en vez de parametrizar con lt podemos hacerlos con Ordering de math Ordering
  import math.Ordering

  //def msort5[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
  def msort5[T](xs: List[T])(ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) => if (ord.lt(x, y))
            x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
        }

      val (fst, snd) = xs splitAt n
      merge(msort5(fst)(ord), msort5(snd)(ord))
    }
  }                                               //> msort5: [T](xs: List[T])(ord: scala.math.Ordering[T])List[T]

  //ponemos el ordering de los enteros
  msort5(myList)(Ordering.Int)                    //> res6: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  //ponemos el ordering de las string
  msort5(fruits)(Ordering.String)                 //> res7: List[String] = List(apple, banana, pear)

  //si utilizamos implicint en el parametro ord nos evitamos tener que ponerlo en la invocacion:
  def msort6[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
   
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) => if (ord.lt(x, y))
            x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
        }

      val (fst, snd) = xs splitAt n
     // merge(msort6(fst)(ord), msort6(snd)(ord)) quitamos el ord
     merge(msort6(fst), msort6(snd))
    }
  }                                               //> msort6: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]
 // no hace falta poner el orden
  msort6(myList)                                  //> res8: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  msort6(fruits)                                  //> res9: List[String] = List(apple, banana, pear)
  
  
}