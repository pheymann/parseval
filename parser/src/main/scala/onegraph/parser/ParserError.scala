package onegraph.parser

import onegraph.parser.internal.ParserResult

trait ParserError

final case class NotEnoughCharacters(actual: Int, expected: Int) extends ParserError

final case class FailedParserWithMsg(msg: String, cause: ParserResult[Any]) extends ParserError
