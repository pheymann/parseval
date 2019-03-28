package onegraph.parser.internal

import java.util.NoSuchElementException

sealed trait ParserState[+A] {

  def isSuccess: Boolean

  def get: A
}

object ParserState {

  final case class Success[A](a: A) extends ParserState[A] {
    def isSuccess = true

    def get = a
  }

  final case class Failed(error: ParserError) extends ParserState[Nothing] {
    def isSuccess = false

    def get = throw new NoSuchElementException("Failed-ParserState has no element")
  }

  trait ParserError
}
