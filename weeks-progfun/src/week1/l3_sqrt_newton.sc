package week1

object l3_sqrt_newton {
  println("newton sqrt")                          //> newton sqrt
  1 + 8                                           //> res0: Int(9) = 9
  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double

  abs(-1)                                         //> res1: Double = 1.0
  abs(1)                                          //> res2: Double = 1.0

  //compute sqrtIter (x)
  // > Start with an initial estimate y (ex: y=1)
  // > Improve the estimate taking the mean of y and x/y
  
  def sqrt(x: Double) = {
    
    def sqrIter(aproximation: Double): Double =
      if (isGoodEnough(aproximation)) aproximation
      else sqrIter(improve(aproximation))

    def isGoodEnough(aproximation: Double) =
      abs(aproximation * aproximation - x) / x < 0.00001
   
    def improve(aproximation: Double) = (aproximation + (x / aproximation)) / 2
   
    sqrIter(1.0)
  }                                               //> sqrt: (x: Double)Double
 
  sqrt(64)                                        //> res3: Double = 8.000001655289593
}