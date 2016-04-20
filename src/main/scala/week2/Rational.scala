package week2

/**
  * Created by lukas on 17.04.16.
  */
class Rational(x: Int, y: Int) {

  require( y != 0, "Denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a % b)

  private val g = gcd(x, y)

  def numer = x / g
  def denom = y / g

  def + (other: Rational) =
    new Rational(numer * other.denom + denom * other.numer,
      denom * other.denom)

  def unary_- = new Rational(-numer, denom)

  def - (other: Rational) = this + -other

  def <(other: Rational): Boolean = numer * other.denom < other.numer * denom

  def max(other: Rational): Rational = if (this < other) other else this

  override def toString = numer + "/" + denom

}
