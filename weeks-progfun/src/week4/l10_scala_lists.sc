package week4

object l10_lists {
  println("lists")                                //> lists
  // estas serian lista de la leccion l1_list, la primera creo que no funcionaria bien pq el aply en el object List2 para 3 parametros no esta definido
  val k = List2 ("aples", "oranges", "pears")     //> k  : week4.Cons1[(String, String, String)] = week4.Cons1@5143dd30
  val kk = List2 ("oranges", "apples")            //> kk  : week4.Cons1[String] = week4.Cons1@3e6ea3fe
  var kkk = List2 ("pine")                        //> kkk  : week4.Cons1[String] = week4.Cons1@51d098b7
  
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
 nums1.toString                                   //> res0: String = List(1, 2, 3)
 
 fruit.head == "apples"                           //> res1: Boolean = true
 fruit.tail.head == "oranges"                     //> res2: Boolean = true
 diag3 == List (1,0,0)                            //> res3: Boolean = false
// empty.head == throw new NoSuchElementException

val l1 = 1::2::Nil                                //> l1  : List[Int] = List(1, 2)
val l2 = 3::4::Nil                                //> l2  : List[Int] = List(3, 4)

//ojo!!! no es lo mismo
val res1 = l1 :: l2                               //> res1  : List[Any] = List(List(1, 2), 3, 4)
val res2 = l1 ::: l2                              //> res2  : List[Int] = List(1, 2, 3, 4)

}