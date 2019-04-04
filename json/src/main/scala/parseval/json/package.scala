package parseval

import parseval.parser.{Parser, ParserResult}

package object json {

  import Parser._

  object parsers {

    val quotedString = lexeme(doubleQuote right string left doubleQuote)
    val jsString = quotedString.map(JsString)
    val jsNumber = lexeme(double).map(JsNumber)
    val jsTrue = lexeme(literal("true")).map(_ => JsTrue)
    val jsFalse = lexeme(literal("false")).map(_ => JsFalse)
    val jsNull = lexeme(literal("null")).map(_ => JsNull)

    private def keyValue: Parser[(String, JsValue)] = for {
      key <- whitespaces right quotedString left lexeme(char(':'))
      value <- jsValue
    } yield key -> value

    def jsObject: Parser[JsObject] = bracket(
      char('{'),
      many(keyValue left lexeme(comma)).map(fields => JsObject(fields.toMap)),
      char('}')
    )

    def jsArray: Parser[JsArray] = bracket(
      char('['),
      commaSep(jsValue).map(values => JsArray(values.toArray)),
      char(']')
    )

    def jsValue: Parser[JsValue] =
      jsString |
        jsNumber |
        jsTrue |
        jsFalse |
        jsNull |
        jsObject |
        jsArray
  }

  def parse(raw: String): ParserResult[JsValue] = {
    import parsers._

    (whitespaces right jsValue left whitespaces).evalConsumeStream(raw.toVector)
  }
}
