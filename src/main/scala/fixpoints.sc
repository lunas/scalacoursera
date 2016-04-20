import math.abs

object session {

  val tolerance = 0.0000001

  def isCloseEnough(x: Double, y: Double) = abs((x-y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {

    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }

    iterate(firstGuess)
  }

  def avgDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  fixedPoint( x => 1 + x/2 )(0.1)

  def sqrt(x: Double) = fixedPoint( avgDamp(y => x/y) )(1)

  sqrt(81)
  sqrt(9)
  sqrt(2)



  avgDamp( x => 2*x)(1)




}