
object test {
  println("shalom")
  val f: PartialFunction[String,String] = { case "ping" => "pong" }
  f("ping")
  f.isDefinedAt("pung")
  val f1: PartialFunction[ List[Int], String] = {
    case Nil => "one"
    case x :: y :: rest => 2*x + "-" + 2*y + "-" + rest
  }

  f1.isDefinedAt(List(1, 2, 3))
  f1(List(3,4,5,6))
  val f2: PartialFunction[ List[Int], String ] = {
    case Nil => "one"
    case x :: rest =>
      rest match {
        case Nil => "two"
      }
  }

  f2.isDefinedAt( List(1,2,3) )
  def even(x: Int) = { x % 2 == 0 }
  (1 until 4) flatMap (i =>
    (1 until i) filter (j => even(i + j)) map
      (j => (i, j)))

  for {
    i <- 1 until 4
    j <- 1 until i
    if even(i + j)
  } yield (i, j)

  def f3(x: Int) = if (x > 2) Some(x) else None
  val l = List( List(1,11), List(2,22), List(3,33))
  l.map (_.reverse)
  l.flatMap( _.reverse )
  val s = List("blue", "red", "green")
  s.map ( _.toUpperCase )
  s.flatMap( _.toUpperCase )


  val m = Map( 1 -> 10, 2 -> 20, 3 -> 30)

  val o = Option[Int](3)
  o.flatMap{
    x => Some(x * 3)
  }


}

