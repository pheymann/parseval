package parseval

import parseval.parser.{Parser, ParserResult}

package object json {

  import Parser._

  object parsers {

    val quotedString = lexeme(doubleQuote right string left doubleQuote)

    val jsString     = quotedString.map(JsString)                       withError "failed to read JsString"
    val jsNumber     = lexeme(double).map(JsNumber)                     withError "failed to read JsNumber"
    val jsTrue       = lexeme(literal("true")).map(_ => JsTrue)    withError "failed to read JsTrue"
    val jsFalse      = lexeme(literal("false")).map(_ => JsFalse)  withError "failed to read JsFalse"
    val jsNull       = lexeme(literal("null")).map(_ => JsNull)    withError "failed to read JsNull"

    private val keyValue: Parser[(String, JsValue)] =
      for {
        key   <- whitespaces right quotedString left lexeme(char(':'))  withError "failed to read key from JsObject"
        value <- jsValue
      } yield key -> value

    lazy val jsObject: Parser[JsObject] = bracket(
      char('{'),
      many(keyValue left lexeme(comma)).map(fields => JsObject(fields.toMap)),
      char('}')
    ) withError "failed to read JsObject"

    lazy val jsArray: Parser[JsArray] = bracket(
      char('['),
      // pure is needed to make `commaSep` lazy
      pure(()).flatMap(_ => commaSep(jsValue).map(values => JsArray(values.toArray))),
      char(']')
    ) withError "failed to read JsArray"

    lazy val jsValue: Parser[JsValue] =
      (
        jsNull
          | jsTrue
          | jsFalse
          | jsString
          | jsNumber
          | jsObject
          | jsArray
      ) withError "failed to read any JsValue type"
  }

  def parse(raw: String): ParserResult[JsValue] = {
    import parsers._

    (whitespaces right jsValue left whitespaces).evalConsumeStream(raw.toVector)
  }
}
