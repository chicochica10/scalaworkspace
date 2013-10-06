package week3

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