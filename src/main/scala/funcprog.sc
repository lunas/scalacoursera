import scala.annotation.tailrec
object session {
  def sqrt(x: Double): Double = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter( improve(guess))
    def isGoodEnough(guess: Double): Boolean =
      Math.abs(guess * guess - x) < 0.001 * x
    def improve(guess: Double): Double =
      (guess + x / guess) / 2
    sqrtIter(1.0)
  }
  val a = sqrt(9)
  a * a

  def fact(n: Int): Int = {

    @tailrec
    def rfact(c: Int, acc: Int): Int =
      if   (c < 1) 1 * acc
      else rfact(c - 1, acc * (c + 1))
    rfact(n - 1, 1)
  }

  fact(5)
  fact(1)
  fact(2)
  fact(3)
  fact(4)




  def ffact(n: Int): Int =
    if (n<2) 1
    else n * ffact(n-1)

  ffact(4)
  ffact(1)


  def sum(f: Int => Int, a: Int, b: Int): Int = {

    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else       loop(a+1, f(a) + acc)
    }

    loop(a, 0)
  }

  sum( (x: Int) => x/2, 1, 3)

  def product( f:Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }
  product( fact )(2,4)
  fact(2)
  fact(3)
  fact(4)
  product( x => x + 1 ) (3, 5)
  product( x => x * x ) (3, 4)

  def profact(n: Int): Int = product( x=>x )(2, n)
  profact(4)
  profact(5)
  profact(1)
  profact(0)



}