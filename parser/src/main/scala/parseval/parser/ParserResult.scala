package parseval.parser

import java.util.NoSuchElementException

sealed trait ParserResult[+A] {

  def isSuccess: Boolean

  def get: A

  def toEither: Either[ParserError, A]
}

object ParserResult {

  final case class Success[A](a: A) extends ParserResult[A] {
    override def isSuccess = true

    override def get = a

    override def toEither: Either[ParserError, A] = Right(a)
  }

  final case class Failed(error: ParserError) extends ParserResult[Nothing] {
    override def isSuccess = false

    override def get = throw new NoSuchElementException("Failed-ParserState has no element")

    override def toEither: Either[ParserError, Nothing] = Left(error)
  }
}
