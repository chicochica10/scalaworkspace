package week4

object insertion_sort_in_lists {
  println("insertion_sort_in_lists")              //> insertion_sort_in_lists

  def isort1(xs: List[Int]): List[Int] = xs match {
    case List() => List() //si es la lista vacia la devuelvo
    case y :: ys => insert(y, isort1(ys))
  }                                               //> isort1: (xs: List[Int])List[Int]

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x > y) y::insert(x,ys) else x::xs
  }                                               //> insert: (x: Int, xs: List[Int])List[Int]
  
  def sort = isort1(List(9, 8, 5, 6, 7, 1, 3, 2, 4))
                                                  //> sort: => List[Int]
sort.toString                                     //> res0: String = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
}