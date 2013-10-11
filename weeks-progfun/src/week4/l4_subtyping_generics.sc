package week4

import week4._

object l4_subtyping_generics {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  //toma un IntSet retorna el IntSet si todos los elementos del int set son positivos
  // arroja una excepcion si no
  def assertAllPos0(s: IntSet): IntSet = ???      //> assertAllPos0: (s: week4.IntSet)week4.IntSet
  //                            ^no es lo bastante preciso pq si devuelve el IntSet deberia ser solo de los positivos
  // esta gobernada por dos ecuaciones:
  // assertAllPos (Empty) = Empty
  //               v NonEmpty(elem: Int, left: IntSet, right: IntSet)
  // assertAllPos (NonEmpty) = NonEmpty
  //                         = throw new error ...
  // para reflejar que devuelve el conjunto de los enteros positivos (podria devolver  Empty, NonEmpty, IntSet)
  def assertAllPos1[S <: IntSet](r: S): S = ???   //> assertAllPos1: [S <: week4.IntSet](r: S)S
  // S es un suptip del conjunto de los enteros

  // tambien podemos decir
  //[S >: NonEmpty] S es un supertipo con lo que puede ser NonEmpty, IntSet, AnyRef, Any (pero no podria ser Empty
  // y se pueden mezclar

  // solo devuleve IntSet y NonEmpty
  def assertAllPos[S >: NonEmpty <: IntSet](r: S): S = ???
                                                  //> assertAllPos: [S >: week4.NonEmpty <: week4.IntSet](r: S)S
  ///covariant ////////////////////////////////////////

  // NonEmpty <: IntSet NonEmpty es un subtipo de IntSet
  // implica que
  //List[NonEmpty]  <: List[IntSet] LAS LISTAS SON COVARIANTES (AL MENOS LA CREADA POR NOSOTROS)
  List[NonEmpty]                                  //> res0: week4.Nil1[Nothing] = week4.Nil1@6762ffa1
  List[IntSet]                                    //> res1: week4.Nil1[Nothing] = week4.Nil1@147cd80e


  //principio de sustitucion de liskov
  // NonEmpty <: IntSet NonEmpty es un subtipo de Intset => que no puedo asignar al array del supertipo un elemento del subtipo
  // LOS ARRAYS EN SCALA NO SON COVARIANTES
  val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty))
                                                  //> a  : Array[week4.NonEmpty] = Array({.1.})
  //val b: Array[IntSet] = a //no puedo asignar
  // b(0) = Empty
  val s: NonEmpty = a(0)                          //> s  : week4.NonEmpty = {.1.}

// pero al revés tampoco
  val a1: Array[IntSet] = Array(new NonEmpty(1, Empty, Empty))
                                                  //> a1  : Array[week4.IntSet] = Array({.1.})
  //val b1: Array[NonEmpty] = a1
  //b1(0) = Empty
 // val s1: NonEmpty = a1(0)
}