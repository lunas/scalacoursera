trait Listl[T] {
  def isEmpty: Boolean
  def head: T
  def tail: Listl[T]
  def nth(i: Int): T
}

class Cons[T](val head: T, val tail: Listl[T]) extends Listl[T] {
  def isEmpty = false

  def nth(i: Int): T = if (i == 0) head else tail.nth(i - 1)
}

class Nil[T] extends Listl[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
  def nth(i: Int)  = throw new IndexOutOfBoundsException("Empty list")
}


val l = new Cons(22, new Cons(33, new Cons(44, new Nil)))

l.nth(0)
l.nth(3)


