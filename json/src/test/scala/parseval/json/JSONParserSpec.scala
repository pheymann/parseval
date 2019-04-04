package parseval.json

import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import parseval.parser.Parser.{CharStreamNotEmpty, FailedCondition}
import parseval.parser.{FailedParserWithStack, ParserError, ParserResult}

object JSONParserSpec extends Specification {

  sequential

  "JsNull" >> {
    parseSuccess("null", JsNull)
    parseSuccess("  null  ", JsNull)

    parseFailed("null0", CharStreamNotEmpty(Vector('0'), ParserResult.Success(JsNull)))
    parseFailed("0null", CharStreamNotEmpty("null".toVector, ParserResult.Success(JsNumber(0))))
    parseFailed("Null", CharStreamNotEmpty("Null".toVector, ParserResult.Failed(
      FailedParserWithStack(
        FailedCondition("N".toVector),
        Vector("not equals to '['", "failed to read JsArray", "failed to read any JsValue type"))
    )))
  }

  "JsTrue" >> {
    parseSuccess("true", JsTrue)
    parseSuccess("  true  ", JsTrue)

    parseFailed("true0", CharStreamNotEmpty(Vector('0'), ParserResult.Success(JsTrue)))
    parseFailed("0true", CharStreamNotEmpty("true".toVector, ParserResult.Success(JsNumber(0))))
    parseFailed("True", CharStreamNotEmpty("True".toVector, ParserResult.Failed(
      FailedParserWithStack(
        FailedCondition("T".toVector),
        Vector("not equals to '['", "failed to read JsArray", "failed to read any JsValue type"))
    )))
  }

  "JsFalse" >> {
    parseSuccess("false", JsFalse)
    parseSuccess("  false  ", JsFalse)

    parseFailed("false0", CharStreamNotEmpty(Vector('0'), ParserResult.Success(JsFalse)))
    parseFailed("0false", CharStreamNotEmpty("alse".toVector, ParserResult.Success(JsNumber(0))))
    parseFailed("False", CharStreamNotEmpty("False".toVector, ParserResult.Failed(
      FailedParserWithStack(
        FailedCondition("F".toVector),
        Vector("not equals to '['", "failed to read JsArray", "failed to read any JsValue type"))
    )))
  }

  "JsString" >> {
    parseSuccess("\"\"", JsString(""))
    parseSuccess("\"hello1234\\\"\"", JsString("hello1234\""))
  }

  private def parseSuccess[A <: JsValue](raw: String, result: A): MatchResult[Any] =
    parse(raw) mustEqual ParserResult.Success(result)

  private def parseFailed[A <: JsValue](raw: String, error: ParserError): MatchResult[Any] =
    parse(raw) mustEqual ParserResult.Failed(error)
}
