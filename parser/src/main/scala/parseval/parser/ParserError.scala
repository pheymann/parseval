package parseval.parser

trait ParserError

final case class NotEnoughCharacters(actual: Int, expected: Int) extends ParserError

final case class FailedParserWithMsg(msg: String, cause: ParserResult[Any]) extends ParserError
