package onegraph.parser.util

case class NonEmptySeq[A](head: A, tail: Seq[A]) {

  def toSeq: Seq[A] = head +: tail
}
