package week4

trait Expr {
  def isNumber: Boolean //classification
  def isSum: Boolean //classification
  def numValue: Int //accesor
  def leftOp: Expr //accesor
  def rightOp: Expr //accesor
}

class Number(n: Int) extends Expr {
  def isNumber = true
  def isSum = false
  def numValue = n
  def leftOp = throw new Error("Number.leftOp")
  def rightOp = throw new Error("Number.rightOp")
}

class Sum(e: Expr, e2: Expr) extends Expr {
  def isNumber = false
  def isSum = true
  def numValue = throw new Error("Sum.numValue")
  def leftOp = e
  def rightOp = e2
}

object l5_decomposition {
  def eval(e: Expr): Int = {
    if (e.isNumber) e.numValue
    else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
    else throw new Error("Unkown expression")
  }                                               //> eval: (e: week4.Expr)Int

  eval(new Sum(new Number(1),new  Number(2)))     //> res0: Int = 3
  
  // no escala si por ejemplo queremos añadir el prod o la var entre los
  // habria que definir 25 nuevos métodos
  // tiene una explosion cuadrática
  
}