import week2.Rational

object session {
  val x = new Rational(1, 2)
  x.numer
  x.denom
  val y = new Rational(2, 4)

  x + y

  -x

  x - x

  val a = new Rational(1, 3)
  val b = new Rational(5, 7)
  val c = new Rational(3, 2)

  a- b - c

  a < c
  b < a
  a.max(b)

}

