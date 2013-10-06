package week1

object l2_factorial {
  println("factorial")                            //> factorial

  def factorial(n: Int): Int =
    if (n == 0) 1 else n * factorial(n - 1)       //> factorial: (n: Int)Int

  factorial(0)                                    //> res0: Int = 1
  factorial(3)                                    //> res1: Int = 6
  factorial(4)                                    //> res2: Int = 24

  def tailFactorial(n: Int): Int = {

    def loop(acc: Int, n: Int): Int =
    	if (n == 0) acc else  loop(acc * n, n - 1)
  
    loop(1, n)
  }                                               //> tailFactorial: (n: Int)Int
  
  tailFactorial (4)                               //> res3: Int = 24
}