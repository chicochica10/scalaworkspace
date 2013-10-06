package week2

object l2_product {
  println("Product")                              //> Product
  //1.-exercice product function
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1 else f(a) * product(f)(a + 1, b)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int

  product(x => x)(1, 4)                           //> res0: Int = 24
  1 * 2 * 3 * 4                                   //> res1: Int = 24
  product(x => x * x * x)(1, 4)                   //> res2: Int = 13824
  1 * 1 * 1 * 2 * 2 * 2 * 3 * 3 * 3 * 4 * 4 * 4   //> res3: Int = 13824

  def fact(n: Int): Int =
    if (n == 0) 1 else n * fact(n - 1)            //> fact: (n: Int)Int

  product(fact)(1, 4)                             //> res4: Int = 288

  fact(1) *
    fact(2) *
    fact(3) *
    fact(4)                                       //> res5: Int = 288

  //2.-exercice definition of factorial in terms of product)
  def factorial(n: Int): Int = product(x => x)(1, n)
                                                  //> factorial: (n: Int)Int

  factorial(4)                                    //> res6: Int = 24

  //3.-exercice generalization of sum and producto
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
                                                  //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int
  // uso
  def myProduct(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x * y, 1)(a, b)        //> myProduct: (f: Int => Int)(a: Int, b: Int)Int

  myProduct(x => x * x * x)(1, 4)                 //> res7: Int = 13824

  def mySum(f: Int => Int)(a: Int, b: Int) : Int =
  	mapReduce (f, (x,y)=>x+y, 0)(a, b)        //> mySum: (f: Int => Int)(a: Int, b: Int)Int
  	
  mySum (x => x * x * x)(1,4)                     //> res8: Int = 100
  
  // o bien directamente
  
  mapReduce (x=>x * x * x, (x,y)=>x+y, 0)(1,4)    //> res9: Int = 100

}