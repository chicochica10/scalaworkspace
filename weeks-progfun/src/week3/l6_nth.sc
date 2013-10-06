package week3

import week3._

object l6_nth {
  println("nth")                                  //> nth
  //      v importante que la funcion tenga el tipo generio que va a de devolver o con el que va a tratar
  def nth[T] (n: Int, xs: List[T]):T =
  	if (xs isEmpty) throw new IndexOutOfBoundsException
  	else if (n==0) xs.head
  	else /*if (n>0)*/ nth (n-1, xs.tail)      //> nth: [T](n: Int, xs: week3.List[T])T
  //else throw new IndexOutOfBoundsException ("n <0")
  		
  val list = new Cons(1, new Cons (2, new Cons (3, new Nil)))
                                                  //> list  : week3.Cons[Int] = week3.Cons@690cbe41
  
  
  nth (2, list)                                   //> res0: Int = 3
 	//nth (-1, list)
   nth (5,list)                                   //> java.lang.IndexOutOfBoundsException
                                                  //| 	at week3.l6_nth$$anonfun$main$1.nth$1(week3.l6_nth.scala:9)
                                                  //| 	at week3.l6_nth$$anonfun$main$1.apply$mcV$sp(week3.l6_nth.scala:19)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at week3.l6_nth$.main(week3.l6_nth.scala:5)
                                                  //| 	at week3.l6_nth.main(week3.l6_nth.scala)
  
}