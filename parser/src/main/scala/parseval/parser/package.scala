package parseval

package object parser {

  type CharStream = Vector[Char]

  object CharStream {

    val empty = Vector.empty[Char]
  }
}
