package parseval.parser

import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import parseval.parser.util.NonEmptySeq

object ParserSpec extends Specification {

  import Parser._

  sequential

  "primitives" >> {
    "satisfies" >> {
      evalAndCheckSuccess(satisfies(_ == 'h'), "h", "", 'h')
      evalAndCheckSuccess(satisfies(_ == 'h'), "hi", "i", 'h')
      evalAndCheckFailed(satisfies(_ == 'h'), "no", "no", FailedCondition(Vector('n')))

      evalAndCheckSuccess(satisfies(2, _ == "hi".toVector), "hi", "", Vector('h', 'i'))
      evalAndCheckSuccess(satisfies(2, _ == "hi".toVector), "hiho", "ho", Vector('h', 'i'))
      evalAndCheckFailed(satisfies(2, _ == "hi".toVector), "ho", "ho", FailedCondition(Vector('h', 'o')))
    }

    "brackets" >> {
      val bracketLeft = satisfies(_ == '(')
      val bracketRight = satisfies(_ == ')')

      evalAndCheckSuccess(bracket(bracketLeft, satisfies(_ == 'h'), bracketRight), "(h)", "", 'h')
      evalAndCheckFailed(bracket(bracketLeft, satisfies(_ == 'h'), bracketRight), ")h)", ")h)", FailedCondition(Vector(')')))
      evalAndCheckFailed(bracket(bracketLeft, satisfies(_ == 'h'), bracketRight), "(h(", "(", FailedCondition(Vector('(')))
      evalAndCheckFailed(bracket(bracketLeft, satisfies(_ == 'h'), bracketRight), "(n)", "n)", FailedCondition(Vector('n')))
    }

    "many" >> {
      val parser = satisfies(_ == 'h')

      evalAndCheckSuccess(many(parser), "hhh", "", Seq('h', 'h', 'h'))
      evalAndCheckSuccess(many(parser), "hha", "a", Seq('h', 'h'))
      evalAndCheckSuccess(many(parser), "a", "a", Seq.empty)

      evalAndCheckSuccess(Parser.atLeastOnce(parser), "hha", "a", NonEmptySeq('h', Seq('h')))
      evalAndCheckFailed(Parser.atLeastOnce(parser), "a", "a", FailedCondition(Vector('a')))
    }

    "skip" >> {
      val parser = satisfies(_ == 'h')

      evalAndCheckSuccess(skip(parser), "hha", "ha", ())
      evalAndCheckFailed(skip(parser), "a", "a", FailedCondition(Vector('a')))

      evalAndCheckSuccess(skipMany(parser), "hha", "a", ())
      evalAndCheckSuccess(skipMany(parser), "a", "a", ())
    }

    "oneOf" >> {
      val parser = Parser.oneOf(satisfies(_ == 'h'), satisfies(_ == 'n'))

      evalAndCheckSuccess(parser, "h", "", 'h')
      evalAndCheckSuccess(parser, "n", "", 'n')
      evalAndCheckFailed(parser, "o", "o", FailedCondition(Vector('o')))
    }
  }

  "char" >> {
    "space" >> {
      evalAndCheckSuccess(space, " ", "", ())
      evalAndCheckSuccess(space, "  ", " ", ())
      evalAndCheckFailed(space, "1", "1", FailedParserWithMsg("not a space", ParserResult.Failed(FailedCondition(Vector('1')))))
      evalAndCheckSuccess(spaces, "  ", "", ())
    }

    "whitespace" >> {
      evalAndCheckSuccess(whitespace, " ", "", ())
      evalAndCheckSuccess(whitespace, "  ", " ", ())
      evalAndCheckFailed(whitespace, "1", "1", FailedParserWithMsg("not a whitespace", ParserResult.Failed(FailedCondition(Vector('1')))))
      evalAndCheckSuccess(whitespaces, "  ", "", ())
    }

    "letter" >> {
      evalAndCheckSuccess(letter, "a", "", 'a')
      evalAndCheckSuccess(letter, "ab", "b", 'a')
      evalAndCheckFailed(letter, "1", "1", FailedParserWithMsg("not a letter", ParserResult.Failed(FailedCondition(Vector('1')))))
      evalAndCheckSuccess(letters, "ab", "", Seq('a', 'b'))
    }

    "digit" >> {
      evalAndCheckSuccess(digit, "1", "", '1')
      evalAndCheckSuccess(digit, "12", "2", '1')
      evalAndCheckFailed(digit, "a", "a", FailedParserWithMsg("not a digit", ParserResult.Failed(FailedCondition(Vector('a')))))
      evalAndCheckSuccess(digits, "12", "", Seq('1', '2'))
    }

    "optimized oneOf for Char" >> {
      evalAndCheckSuccess(oneOfChar('a', 'b'), "a", "", 'a')
      evalAndCheckSuccess(oneOfChar('a', 'b'), "b", "", 'b')
      evalAndCheckFailed(oneOfChar('a', 'b'), "c", "c", FailedParserWithMsg("not in [a, b]", ParserResult.Failed(FailedCondition(Vector('c')))))
    }

    "literal" >> {
      evalAndCheckSuccess(literal("hello"), "hello", "", "hello")
      evalAndCheckSuccess(literal("hello"), "hello, you", ", you", "hello")
      evalAndCheckFailed(literal("hello"), "yello", "yello", FailedParserWithMsg("not equal to \"hello\"", ParserResult.Failed(FailedCondition("yello".toVector))))
    }
  }

  "number" >> {
    "natural" >> {
      evalAndCheckSuccess(natural, "1", "", 1)
      evalAndCheckSuccess(natural, "11", "", 11)
      evalAndCheckSuccess(natural, "1a", "a", 1)
      evalAndCheckFailed(natural, "-1", "-1", FailedParserWithMsg("not a digit", ParserResult.Failed(FailedCondition(Vector('-')))))
    }

    "integer" >> {
      evalAndCheckSuccess(int, "1", "", 1)
      evalAndCheckSuccess(int, "11", "", 11)
      evalAndCheckSuccess(int, "-1", "", -1)
      evalAndCheckSuccess(int, "1.0", ".0", 1)
    }

    "long" >> {
      evalAndCheckSuccess(long, "1", "", 1l)
      evalAndCheckSuccess(long, "1l", "", 1l)
      evalAndCheckSuccess(long, "1L", "", 1l)
      evalAndCheckSuccess(long, "11l", "", 11l)
      evalAndCheckSuccess(long, "-1", "", -1l)
      evalAndCheckSuccess(long, "1.0", ".0", 1l)
    }

    "float" >> {
      evalAndCheckSuccess(float, "1", "", 1f)
      evalAndCheckSuccess(float, "1f", "", 1f)
      evalAndCheckSuccess(float, "1F", "", 1f)
      evalAndCheckSuccess(float, "1.0", "", 1f)
      evalAndCheckSuccess(float, "1.0f", "", 1f)
      evalAndCheckSuccess(float, "-1.0f", "", -1f)
      evalAndCheckSuccess(float, "11e2", "", 1100f)
      evalAndCheckSuccess(float, "11E2", "", 1100f)
      evalAndCheckSuccess(float, "11E-2", "", 0.11f)
      evalAndCheckSuccess(float, "1.0d", "d", 1.0f)

      val (remaining, result) = float.eval("1.111f".toVector)

      remaining                 mustEqual "".toVector
      result.isSuccess          must beTrue
      (result.get - 1.111f).abs must lessThanOrEqualTo(0.0001f)
    }

    "double" >> {
      evalAndCheckSuccess(double, "1", "", 1d)
      evalAndCheckSuccess(double, "1d", "", 1d)
      evalAndCheckSuccess(double, "1F", "", 1d)
      evalAndCheckSuccess(double, "1.0", "", 1d)
      evalAndCheckSuccess(double, "1.0d", "", 1d)
      evalAndCheckSuccess(double, "-1.0d", "", -1d)
      evalAndCheckSuccess(double, "11e2", "", 1100d)
      evalAndCheckSuccess(double, "11E2", "", 1100d)
      evalAndCheckSuccess(double, "11E-2", "", 0.11d)

      val (remaining, result) = double.eval("1.111d".toVector)

      remaining                 mustEqual "".toVector
      result.isSuccess          must beTrue
      (result.get - 1.111d).abs must lessThanOrEqualTo(0.0001d)
    }
  }

  "token" >> {
    "words" >> {
      evalAndCheckSuccess(word, "hello", "", "hello")
      evalAndCheckSuccess(word, "hello    ", "", "hello")
    }
  }

  private def evalAndCheckSuccess[A](parser: Parser[A],
                                     input: String,
                                     remaining: String,
                                     result: A): MatchResult[Any] = {
    parser.eval(input.toVector) mustEqual (remaining.toVector, ParserResult.Success(result))
  }

  private def evalAndCheckFailed[A](parser: Parser[A],
                                    input: String,
                                    remaining: String,
                                    error: ParserError): MatchResult[Any] = {
    parser.eval(input.toVector) mustEqual (remaining.toVector, ParserResult.Failed(error))
  }
}
