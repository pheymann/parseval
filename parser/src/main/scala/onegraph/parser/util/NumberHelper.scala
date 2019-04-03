package onegraph.parser.util

trait NumberHelper[A] {

  def zero: A

  def multByTen(x: A): A

  def addWithInt(x: A, y: Int): A

  def negate(a: A): A
}

object NumberHelper {

  @inline def apply[A: NumberHelper]: NumberHelper[A] = implicitly[NumberHelper[A]]
}
