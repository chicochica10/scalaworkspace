package week2

object l2_currying {
	 println("currying")                      //> currying
  //                    v  devuelve una funcion que tiene dos parametros y devuelve un entero
  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
    	if (a>b) 0 else f(a) + sumF(a+1,b)
				
    sumF
  }                                               //> sum: (f: Int => Int)(Int, Int) => Int
  // uso:
  sum (x=>x*x*x)(1,10)                            //> res0: Int = 3025
  // o bien:
  
  def sumInts = sum (x=>x)                        //> sumInts: => (Int, Int) => Int
  def sumCubes = sum (x=>x*x*x)                   //> sumCubes: => (Int, Int) => Int
  
  def fact(n: Int): Int = if (n == 0) 1 else n * fact(n - 1)
                                                  //> fact: (n: Int)Int
  def sumFactorials = sum (fact)                  //> sumFactorials: => (Int, Int) => Int
  
  sumInts (1,10)                                  //> res1: Int = 55
  sumCubes (1,10)                                 //> res2: Int = 3025
  sumFactorials (1,10)                            //> res3: Int = 4037913
  
  sumInts(1,10) + sumCubes(1,10)                  //> res4: Int = 3080
  
  // o bien:
  def cube(x: Int) = x * x * x                    //> cube: (x: Int)Int
  
  sum (cube) (1,0)                                //> res5: Int = 0

	// syntactic sugar idem higher_order_funtions
	def sum2(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum2(f)(a + 1, b)    //> sum2: (f: Int => Int)(a: Int, b: Int)Int
  
  sum2(x => x)(1, 10)                             //> res6: Int = 55
  sum2(x => x * x * x)(1, 10)                     //> res7: Int = 3025
  sum2(x => if (x == 0) 1 else x * fact(x - 1))(1, 10)
                                                  //> res8: Int = 4037913
  sum2(fact)(1,10)                                //> res9: Int = 4037913
 
}