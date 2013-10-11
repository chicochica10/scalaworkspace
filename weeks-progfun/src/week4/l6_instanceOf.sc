package week4

trait Expr {
 // def isNumber: Boolean //classification
 // def isSum: Boolean //classification
 // def numValue: Int //accesor
 // def leftOp: Expr //accesor
//  def rightOp: Expr //accesor
}

class Number(n: Int)  extends Expr {
 // def isNumber = true
 // def isSum = false
  def numValue = n
 // def leftOp = throw new Error("Number.leftOp")
 // def rightOp = throw new Error("Number.rightOp")
}

class Sum(e: Expr, e2: Expr)  extends Expr {
 // def isNumber = false
 // def isSum = true
 // def numValue = throw new Error("Sum.numValue")
  def leftOp = e
  def rightOp = e2
}

object instanceOf {
  println("instanceOf")                           //> instanceOf
  // NO RECOMENDADO  low level and unsafe
  // aunque es mejor que el metodo de l5 pq no necesita classification metodo y solo se definen los metodos donde son necesarios
  // x.isInstanceOf[T] -> java x instanceof T
  // x.asInstanceOf[T] -> (T) x
	
  def eval(e: Expr): Int = {
    if (e.isInstanceOf[Number])
      e.asInstanceOf[Number].numValue
    else if (e.isInstanceOf[Sum])
      eval(e.asInstanceOf[Sum].leftOp) +
        eval(e.asInstanceOf[Sum].rightOp)
    else throw new Error("unknown expression " + e)
  }                                               //> eval: (e: week4.Expr)Int

  eval(new Sum(new Number(3), new Number(4)))     //> res0: Int = 7
}