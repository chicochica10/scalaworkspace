package week2

object l4_rationals_ {
  println("rationals")                            //> rationals
  val rational = new Rational(1, 2)               //> rational  : week2.Rational = 1/2
  rational.num                                    //> res0: Int = 1
  rational.den                                    //> res1: Int = 2


  val sum = new Rational(3, 4)                    //> sum  : week2.Rational = 3/4
  val res = rational.add(sum)                     //> res  : week2.Rational = 5/4
  println("sum of " + rational.toString + " plus " + sum.toString + " is " + res.toString)
                                                  //> sum of 1/2 plus 3/4 is 5/4

  val x = new Rational(1, 3)                      //> x  : week2.Rational = 1/3
  val y = new Rational(5, 7)                      //> y  : week2.Rational = 5/7
  val z = new Rational(3, 2)                      //> z  : week2.Rational = 3/2

  //x-y-z
  x.sub(y).add(z.neg).toString                    //> res2: String = -79/42
  x.sub(y).sub(z).toString                        //> res3: String = -79/42

  y.add(y)                                        //> res4: week2.Rational = 10/7

	x.less(y)                                 //> res5: Boolean = true
	x.more(y)                                 //> res6: Boolean = false
	x.equal(x)                                //> res7: Boolean = true
	
	x.max(y)                                  //> res8: week2.Rational = 5/7
	
//	val strange = new Rational (1,0)
//	strange.add(strange)

	var mono= new Rational (2)                //> mono  : week2.Rational = 2/1
	
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

  def neg = new Rational(-x, y)

  def add(that: Rational) =
    new Rational(num * that.den + that.num * den, den * that.den)

  def sub(that: Rational) = add(that.neg)

  def mul(that: Rational) =
    new Rational(num * that.num, den * that.den)

  def div(that: Rational) =
    new Rational(num * that.den, den * that.num)

  def equal(that: Rational) = num * that.den == den * that.num

  def less(that: Rational) = num * that.den < den * that.num

  def more(that: Rational) = !(this.less(that) || this.equal(that))

  def max(that: Rational) = if (this.less(that)) that else this

  def min(that: Rational) = if (this.less(that)) this else that

  override def toString = num + "/" + den
}