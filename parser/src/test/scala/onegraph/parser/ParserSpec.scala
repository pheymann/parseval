package onegraph.parser

import onegraph.parser.internal.ParserResult
import onegraph.parser.util.NonEmptySeq
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

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
