package week2

import math.abs

object l3_fix_point_sqrt_newton {
  println("l3_fix_point_sqrt_newton")             //> l3_fix_point_sqrt_newton
  
  def fixedPoint (f: Double => Double)(firstGuess: Double) =
  {
  	val tolerance = 0.0001
  	def isCloseEnough (x: Double, y: Double): Boolean =
  		abs((x - y) / x) / x < tolerance
  	
  	def iterate (guess: Double): Double = {
  	  println( "guess " + guess)
  		val next = f(guess)
  		if (isCloseEnough(guess, next)) next else
  		iterate (next)
  	}
  	
  	iterate (firstGuess)
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
  
  
  
 
  fixedPoint (x => 1 + x/2)(1)                    //> guess 1.0
                                                  //| guess 1.5
                                                  //| guess 1.75
                                                  //| guess 1.875
                                                  //| guess 1.9375
                                                  //| guess 1.96875
                                                  //| guess 1.984375
                                                  //| guess 1.9921875
                                                  //| guess 1.99609375
                                                  //| guess 1.998046875
                                                  //| guess 1.9990234375
                                                  //| guess 1.99951171875
                                                  //| res0: Double = 1.999755859375
  
  // no converge oscila entre 1 y 2
  def sqrt_1 (x: Double) =
  	fixedPoint(y=> x/y)(1)                    //> sqrt_1: (x: Double)Double
  	
  //sqrt_1 (2)
  
  // hacemos quc converga calculando la media de y e x/y
  def sqrt_2 (x:Double) =
  	fixedPoint (y=> (y + x/y)/2)(1)           //> sqrt_2: (x: Double)Double
  	
  sqrt_2 (2)                                      //> guess 1.0
                                                  //| guess 1.5
                                                  //| guess 1.4166666666666665
                                                  //| guess 1.4142156862745097
                                                  //| res1: Double = 1.4142135623746899
  //sacamos el calculo de la media a una funcion (ojo devuleve una funcion pero podria concretarla dando una x (ver ejemplo)
  def averageDamp (f: Double => Double)(x: Double) =
  	(x + f(x)) / 2                            //> averageDamp: (f: Double => Double)(x: Double)Double
  
  //ejemplo aunque no deja (para que deje hay que hacerlo como en el ejemplo de l2_currying
  //def myMedia = averageDamp (x=>x+2)
  //println (myMedia(1))
  
  //
 	def sqrt (x: Double) =
 		fixedPoint (averageDamp (y=>x/y))(1)
                                                  //> sqrt: (x: Double)Double
  sqrt (2)                                        //> guess 1.0
                                                  //| guess 1.5
                                                  //| guess 1.4166666666666665
                                                  //| guess 1.4142156862745097
                                                  //| res2: Double = 1.4142135623746899
}