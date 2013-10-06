package week2

object l4_rationals_with_operators {
  println("rationals")                            //> rationals
  val rational = new Rational(1, 2)               //> rational  : week2.Rational = 1/2
  rational.num                                    //> res0: Int = 1
  rational.den                                    //> res1: Int = 2

  val sum = new Rational(3, 4)                    //> sum  : week2.Rational = 3/4
  val res = rational + sum                        //> res  : week2.Rational = 5/4
  println("sum of " + rational.toString + " plus " + sum.toString + " is " + res.toString)
                                                  //> sum of 1/2 plus 3/4 is 5/4

  val x = new Rational(1, 3)                      //> x  : week2.Rational = 1/3
  val y = new Rational(5, 7)                      //> y  : week2.Rational = 5/7
  val z = new Rational(3, 2)                      //> z  : week2.Rational = 3/2

  //x-y-z
  x - y - z                                       //> res2: week2.Rational = -79/42
  

  y + y                                           //> res3: week2.Rational = 10/7

	x < y                                     //> res4: Boolean = true
	x > y                                     //> res5: Boolean = false
	x == x                                    //> res6: Boolean = true
	
	x max y                                   //> res7: week2.Rational = 5/7
	
//	val strange = new Rational (1,0)
//	strange.add(strange)

	new Rational (2)                          //> res8: week2.Rational = 2/1
	
}
class Rational(x: Int, y: Int) {
	require (y != 0, "denominator must be nonzero")
	//at this point y is nonzero
	assert (y!=0,"denominator must be nonzero")
	
	//other constructor
	def this(x: Int) = this(x,1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def num = x / g
  def den = y / g

  def unary_- = new Rational(-x, y)

  def + (that: Rational) =
    new Rational(num * that.den + that.num * den, den * that.den)

  def - (that: Rational) = this + -that//add(that.neg)

  def * (that: Rational) =
    new Rational(num * that.num, den * that.den)

  def / (that: Rational) =
    new Rational(num * that.den, den * that.num)

  def == (that: Rational) = num * that.den == den * that.num

  def < (that: Rational) = num * that.den < den * that.num

  def > (that: Rational) = !((this < that) || (this == that))

  def max (that: Rational) = if (this < that) that else this

  def min(that: Rational) = if (this < that) this else that

  override def toString = num + "/" + den
}