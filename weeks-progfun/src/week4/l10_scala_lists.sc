package week4

object l10_lists {
  println("lists")                                //> lists
  // estas serian lista de la leccion l1_list, la primera creo que no funcionaria bien pq el aply en el object List2 para 3 parametros no esta definido
  val k = List2 ("aples", "oranges", "pears")     //> k  : week4.Cons1[(String, String, String)] = week4.Cons1@6d475479
  val kk = List2 ("oranges", "apples")            //> kk  : week4.Cons1[String] = week4.Cons1@65fb0bfb
  var kkk = List2 ("pine")                        //> kkk  : week4.Cons1[String] = week4.Cons1@44cfc00b
  
  //las listas son inmutables y recursivas
  val fruit/*: List[String]*/ = List ("apples", "oranges", "pears")
                                                  //> fruit  : List[String] = List(apples, oranges, pears)
  val nums/*:List [Int]*/ = List (1,2,3)          //> nums  : List[Int] = List(1, 2, 3)
  val diag3/*:List[List[Int]]*/ = List (List(1,0,0), List(0,1,0), List (0,0,1))
                                                  //> diag3  : List[List[Int]] = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
                                                  //| 
  val empty/*:List[Nothing]*/ = List()            //> empty  : List[Nothing] = List()
  
  
  //operacion construccion (cons) es :: la lista vacia es Nil
 val fruit1 = "apple" :: ("oranges" :: ("pears" :: Nil))
                                                  //> fruit1  : List[String] = List(apple, oranges, pears)
 val empty1 = Nil                                 //> empty1  : scala.collection.immutable.Nil.type = List()
 
 // tambien:
 
 val nums1 = 1::2::3::Nil                         //> nums1  : List[Int] = List(1, 2, 3)
 
 fruit.head == "apples"                           //> res0: Boolean = true
 fruit.tail.head == "oranges"                     //> res1: Boolean = true
 diag3 == List (1,0,0)                            //> res2: Boolean = false
// empty.head == throw new NoSuchElementException

}