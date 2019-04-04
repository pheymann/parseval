package parseval

package object parser {

  type CharStream = Vector[Char]

  type ErrorMsg = String

  object CharStream {

    val empty = Vector.empty[Char]
  }
}
