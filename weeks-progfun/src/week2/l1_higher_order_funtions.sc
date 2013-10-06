package week2

object l1_higher_order_funtions {
  println("higher order functions")               //> higher order functions

  def id(x: Int) = x                              //> id: (x: Int)Int

  def sumInts(a: Int, b: Int): Int =
    if (a > b) 0 else id(a) + sumInts(a + 1, b)   //> sumInts: (a: Int, b: Int)Int

  println(sumInts(1, 10))                         //> 55
  1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10          //> res0: Int = 55

  def cube(x: Int) = x * x * x                    //> cube: (x: Int)Int

  cube(2)                                         //> res1: Int = 8

  def sumCubes(a: Int, b: Int): Int =
    if (a > b) 0 else cube(a) + sumCubes(a + 1, b)//> sumCubes: (a: Int, b: Int)Int

  sumCubes(1, 10)                                 //> res2: Int = 3025

  def fact(n: Int): Int =
    if (n == 0) 1 else n * fact(n - 1)            //> fact: (n: Int)Int

  fact(10)                                        //> res3: Int = 3628800

  def sumFactorials(a: Int, b: Int): Int =
    if (a > b) 0 else fact(a) + sumFactorials(a + 1, b)
                                                  //> sumFactorials: (a: Int, b: Int)Int

  sumFactorials(1, 10)                            //> res4: Int = 4037913

  def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum(f, a + 1, b)     //> sum: (f: Int => Int, a: Int, b: Int)Int

  sum(id, 1, 10)                                  //> res5: Int = 55
  sum(cube, 1, 10)                                //> res6: Int = 3025
  sum(fact, 1, 10)                                //> res7: Int = 4037913

// funciones anonimas
  sum(x => x, 1, 10)                              //> res8: Int = 55
  sum(x => x * x * x, 1, 10)                      //> res9: Int = 3025
  sum(x => if (x == 0) 1 else x * fact(x - 1), 1, 10)
                                                  //> res10: Int = 4037913
// syntactic sugar (idem que en currying
  def sum2(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum2(f)(a + 1, b)    //> sum2: (f: Int => Int)(a: Int, b: Int)Int

  sum2(x => x)(1, 10)                             //> res11: Int = 55
  sum2(x => x * x * x)(1, 10)                     //> res12: Int = 3025
  sum2(x => if (x == 0) 1 else x * fact(x - 1))(1, 10)
                                                  //> res13: Int = 4037913

//recursive tail
  def sum3(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a +1 , f(a) + acc)
    }
    loop(a, 0)
  }                                               //> sum3: (f: Int => Int)(a: Int, b: Int)Int


  sum3(x => x)(1, 10)                             //> res14: Int = 55
  sum3(x => x * x * x)(1, 10)                     //> res15: Int = 3025
  sum3(x => if (x == 0) 1 else x * fact(x - 1))(1, 10)
                                                  //> res16: Int = 4037913
}