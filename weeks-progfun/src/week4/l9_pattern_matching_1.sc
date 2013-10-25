package week4

object l9_pattern_matching_1 {
  println("pattern matching")                     //> pattern matching
  // el problema que queremos resolver es
  // dada una expresion que puede ser:
  //            v
  //   Number, sum, prod var
  //
  // aplicar los metodos:
  // var
  // sum
  // prod
  // simplify
  
// tener en cuenta que el proposito de la decomposicion
// es poder luego revertir el proceso de construcion
//  la expresion recogiendo las
// sub expresiones para aplicarles los metodos

trait Expr
case class Number (n: Int) extends Expr // case class
case class Sum (e1: Expr, e2: Expr) extends Expr // case class

// al añadir case el compilador añade los objetos acompañantes
// con sus metodos apply como hicimos manualmente en el object list
// de l1_list.scala

// añadidos automaticamente pero los pinto
// estas comentados pq si no dice que ya existe en la clase Number y sum
// NOTA: son metodos factoria
// object Number {
// 	def apply (n: Int) = new Number (n)
// }

//object Sum {
//	def apply (e1: Expr, e2: Expr) = new Sum (e1, e2)
//}
// con lo que puedo hacer
Number (2) // si quito los case daria un error de compilacion
                                                  //> res0: week4.l9_pattern_matching_1.Number = Number(2)
// se puede poner de manera optativa
Number (n = 2)                                    //> res1: week4.l9_pattern_matching_1.Number = Number(2)
//pero no
//Number (t = 2)
Sum (Number(1),Number (3))                        //> res2: week4.l9_pattern_matching_1.Sum = Sum(Number(1),Number(3))

// asi evitamos el uso de new Number(2)

//PATTERN MATCHING

def eval (e: Expr): Int = e match {
	case Number (n) => n
	case Sum (e1, e2) => eval(e1) + eval (e2)
}                                                 //> eval: (e: week4.l9_pattern_matching_1.Expr)Int

// ejemplo
eval (Sum(Number(1), Number(2)))                  //> res3: Int = 3

// se puede hacer dentro del trait mejor
// verlo en l9_patter_matching_2
}