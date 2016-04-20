abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat

  def count(i: Int): Int = if (isZero) i else predecessor.count(i + 1)
}


object Zero extends Nat {

  def isZero = true
  def predecessor = throw new IllegalStateException("No predecessor for Zero")
  def successor = new Succ(Zero)
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = that
  override def toString = "0"

}
class Succ(n: Nat) extends Nat {

  def isZero = false

  def predecessor = n

  def successor = new Succ(this)

  def + (that: Nat): Nat =
    if (that.isZero) this
    else new Succ(this) + that.predecessor

  def - (that: Nat): Nat =
    if (that.isZero) this
    else this.predecessor - that.predecessor

  override def toString = {
    count(0).toString
  }



}


val a = new Succ(new Succ(new Succ(Zero)))
val b = new Succ(new Succ(Zero))

a + b
a - b

