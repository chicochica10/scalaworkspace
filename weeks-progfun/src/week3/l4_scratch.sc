package week3

//NOTA DE LA LECCION: los traits con como interfaces que admiten metodos y propiedades se implementan con with (pero creo que tienen que extender de otra clase primero)
//se diferencia de la clases scala que no pueden llevar parametros como los lleva p.e la clase Rational

//formas de importar
//import week3.Rational
//import week3.{ Rational, L3_scalaHello }
//import week3._

object l4_scratch {
  println("call to rationals")                    //> call to rationals
  //new week3.Rational (1,2)
  // si se importa
  new Rational(1, 2)                              //> res0: week3.Rational = 1/2

  // devuelve scala.Nothing
  def error(msg: String) = throw new Error(msg)   //> error: (msg: String)Nothing

  // error("test")

  val x = null // valor de scala.Null             //> x  : Null = null
  val y: String = x                               //> y  : String = null

  // val z: Int = x no es posible assignar Null (scala.Null) a un objeto scala.AnyVal pero si a los scala.AnyRef (java.lang.Object)
  
  if (true) 1 else false                          //> res1: AnyVal = 1
}