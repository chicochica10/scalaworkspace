package week4

object l1_fuctions_as_objects {
  println("funcitons as objects")                 //> funcitons as objects

  //         v-------- el tipo de la funcion f es Int => Int que es una abreviatura de de la clase (*)
  def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum(f, a + 1, b)     //> sum: (f: Int => Int, a: Int, b: Int)Int

  sum((x: Int) => x * x, 1, 10)                   //> res0: Int = 385

  // (*)
  // package scala
  // trait Function1 [A,B] {
  //   def apply (x: A): B
  //}

  // asi pues las funciones f son un objeto con un metodo apply
  //Function2, Function3, se aplican a funcionnes con 2, 3 .... parametros (hasta 22)

  // la funcion anónima
  (x: Int) => x * x                               //> res1: Int => Int = <function1>
  // se expande en (**)
  new AnonFun                                     //> res2: week4.AnonFun = <function1>
  // o bien en
  new Function1[Int, Int] {
    def apply(x: Int) = x * x
  }                                               //> res3: Int => Int = <function1>

  // con lo que una funcion del tipo
  val f = (x: Int) => x * x                       //> f  : Int => Int = <function1>
  f(7)                                            //> res4: Int = 49
  // se puede expresar como
  val f1 = new Function1[Int, Int] {
    def apply(x: Int) = x * x
  }                                               //> f1  : Int => Int = <function1>
  f1.apply(7)                                     //> res5: Int = 49
  
  //y OJO!! esto es importante para el ejercicio l1_list f tambien tiene el metodo apply pq su tipo es una funcion (que es una clase Function1)
  f.apply(7)                                      //> res6: Int = 49
  
  ///////////
  // si tenemos un método
  def f2(x: Int): Boolean = ???                   //> f2: (x: Int)Boolean
  // podemos hacer
  (x: Int) => f2(x)                               //> res7: Int => Boolean = <function1>

  // que es igual  como hemos visto a:
  new Function1[Int, Boolean] {
    def apply(x: Int) = f2(x)
  }                                               //> res8: Int => Boolean = <function1>

}
//(**)
class AnonFun extends Function1[Int, Int] {
  def apply(x: Int) = x * x
}
 		
 	