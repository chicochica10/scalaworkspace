package week1

object l1_gcd {
  println("gcd")                                  //> gcd
  def gcd (a: Int, b: Int) : Int =
  	if (b == 0) a else gcd (b, a%b)           //> gcd: (a: Int, b: Int)Int
  
  gcd (10,6)                                      //> res0: Int = 2
  gcd (18,6)                                      //> res1: Int = 6
  gcd (42,56)                                     //> res2: Int = 14
  gcd (48,60)                                     //> res3: Int = 12
  
}