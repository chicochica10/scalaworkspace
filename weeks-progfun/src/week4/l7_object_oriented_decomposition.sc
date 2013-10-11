package week4

//mejor que l6 y l5

trait Expr {
  def eval: Int
  // pero si queremos mostrar expresiones por ejemplo:
  // que tenemos que tocar todas las clases en la jerarquia
  def show: String // nuevo
  // pero si lo querermos no es calcular la expresion si no simplificarla:
  // a * b + a * c -> a * (b + c)
  // no podemos encapsular la operancion como un metodo de un solo objeto
  // involucra a todo el árbol de objetos
}

class Number(n: Int) extends Expr {
  def eval = n
  def show = "" + n // nuevo
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  def eval = e1.eval + e2.eval
  def show = e1.eval + " + " + e2.eval // nuevo
}

object l7_object_oriented_decomposition {
  println("l7 object oriented decomposition")     //> l7 object oriented decomposition
  new Sum(new Number(5), new Number(6)) show      //> res0: String = 5 + 6
}