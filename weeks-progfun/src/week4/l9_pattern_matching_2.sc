package week4

object l10_pattern_maching_2 {
  println("pattern matching 2")                   //> pattern matching 2

  // pattern matching dentro del trait
  trait Expr {
    //def eval (e: Expr): Int = e match {
    def eval: Int = this match {
      case Number(n) => n
      case Sum(e1, e2) => e1.eval + e2.eval //case Sum (e1, e2) => eval(e1) + eval (e2)
    }

    // añadimos show
    def show: String = this match {
      case Number(n) => n.toString
      case Sum(e1, e2) => e1.show + " + " + e2.show
    }

  }
  case class Number(n: Int) extends Expr // case class
  case class Sum(e1: Expr, e2: Expr) extends Expr // case class

  Number(2).eval                                  //> res0: Int = 2
  val suma = Sum(Number(1), Number(3))            //> suma  : week4.l10_pattern_maching_2.Sum = Sum(Number(1),Number(3))
  suma.show + " = " + suma.eval                   //> res1: String = 1 + 3 = 4

}