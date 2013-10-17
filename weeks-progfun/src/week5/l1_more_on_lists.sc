package week5

object l1_more_on_lists {
  println("more on lists")                        //> more on lists
  
  val myList1=List(1,2,3,4,5)                     //> myList1  : List[Int] = List(1, 2, 3, 4, 5)
  val myList2=List(6,7,8,9)                       //> myList2  : List[Int] = List(6, 7, 8, 9)
  
  myList1.last                                    //> res0: Int = 5
  // last
  def last[T](xs: List[T]): T = xs match {
  	case List() => throw new Error ("last of empty list")
  	case List(x) => x
  	case y :: ys => last(ys)
  }                                               //> last: [T](xs: List[T])T
  last(myList1)                                   //> res1: Int = 5
  
  // init
  myList1.init                                    //> res2: List[Int] = List(1, 2, 3, 4)
  def init[T](xs: List[T]): List[T] = xs match {
  	case List() => throw new Error ("init of empty list")
  	case List(x) => List()
  	case y :: ys => y :: init(ys)
  }                                               //> init: [T](xs: List[T])List[T]
  init(myList1)                                   //> res3: List[Int] = List(1, 2, 3, 4)
  
  // Concat xs ::: ys =
  myList1 ::: myList2                             //> res4: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  myList2.::: (myList1)                           //> res5: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  myList1 ++ myList2                              //> res6: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  
  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  	case List() => ys
  	case z :: zs => z :: concat (zs, ys)
  }                                               //> concat: [T](xs: List[T], ys: List[T])List[T]
  
  concat (myList1, myList2)                       //> res7: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  
  //reverse
  myList1.reverse                                 //> res8: List[Int] = List(5, 4, 3, 2, 1)
  // NOTA: SE PUEDE MEJORAR PARA QUE TARDE MENO ahora es n*n
  def reverse[T] (xs: List[T]): List[T] = xs match {
  	case List() => xs
  	case y :: ys => reverse (ys) ++ List(y)
  }                                               //> reverse: [T](xs: List[T])List[T]
  reverse (myList1)                               //> res9: List[Int] = List(5, 4, 3, 2, 1)
  
  //remove
  def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n+1)
                                                  //> removeAt: [T](n: Int, xs: List[T])List[T]
  removeAt (2, myList1)                           //> res10: List[Int] = List(1, 2, 4, 5)
}