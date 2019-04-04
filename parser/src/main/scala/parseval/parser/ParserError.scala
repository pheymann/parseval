package parseval.parser

trait ParserError

final case class NotEnoughCharacters(actual: Int, expected: Int) extends ParserError

final case class FailedParserWithStack(cause: ParserError, stack: Vector[ErrorMsg]) extends ParserError
