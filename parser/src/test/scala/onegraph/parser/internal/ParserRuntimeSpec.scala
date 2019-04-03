package onegraph.parser.internal

import onegraph.parser.Parser
import onegraph.parser.internal.ParserRuntime.FailedParserInternal
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

object ParserRuntimeSpec extends Specification {

  import Parser._

  sequential

  "successfully process Parser tree" >> {
    "pure" >> {
      "single" >> {
        checkSuccess(pure(0), "", 1, "", 0)
        checkSuccess(pure(0), "h", 1, "h", 0)
      }

      "with stack" >> {
        checkSuccess(pure('h').right(pure('i')).map(_ => 0), "hi", 5, "hi", 0)
        checkSuccess(pure('h').right(pure('i')).map(_ => 0), "", 5, "", 0)
      }
    }

    "rule" >> {
      "single" >> {
        checkSuccess(char('h'), "h", 2, "", 'h')
        checkSuccess(char('h'), "hi", 2, "i", 'h')
      }

      "with stack" >> {
        checkSuccess(char('h').right(char('i')).map(_ => 0), "hi", 7, "", 0)
        checkSuccess(char('h').right(char('i')).map(_ => 0), "hi!", 7, "!", 0)
      }
    }
  }

  "failure while processing Parser tree" >> {
    "pure" >> {
      "single" >> {
        checkFailure(failed(TestError), "", 1, "", TestError)
        checkFailure(failed(TestError), "h", 1, "h", TestError)
      }

      "with Stack" >> {
        checkFailure(pure('h').left(failed(TestError)).map(_ => 0), "", 5, "", TestError)
        checkFailure(pure('h').left(failed(TestError)).map(_ => 0), "h", 5, "h", TestError)
      }
    }

    "rule" >> {
      "single" >> {
        checkFailure(char('h'), "c", 2, "c", FailedParserInternal("not equals to 'h'", ParserState.Failed(FailedCondition(Vector('c')))))
        checkFailure(char('h'), "ci", 2, "ci", FailedParserInternal("not equals to 'h'", ParserState.Failed(FailedCondition(Vector('c')))))
        checkFailure(char('h'), "", 2, "", NotEnoughCharacters(0, 1))
      }

      "with stack" >> {
        checkFailure(char('h').right(char('i')).map(_ => 0), "ho", 6, "o", FailedParserInternal("not equals to 'i'", ParserState.Failed(FailedCondition(Vector('o')))))
        checkFailure(char('h').right(char('i')).map(_ => 0), "hola", 6, "ola", FailedParserInternal("not equals to 'i'", ParserState.Failed(FailedCondition(Vector('o')))))
      }
    }
  }

  "OR branching" >> {
    "pure" >> {
      "single" >> {
        checkSuccess(pure(0).or(pure(1)), "", 2, "", 0)
        checkSuccess(pure(0).or(pure(1)), "h", 2, "h", 0)
        checkSuccess(failed(TestError).or(pure(1)), "h", 3, "h", 1)
      }

      "with stack" >> {
        checkSuccess(pure(0).left(pure(1)).or(pure(1)), "", 6, "", 0)
        checkSuccess(pure(0).left(pure(1)).or(pure(1)), "h", 6, "h", 0)
        checkSuccess(pure(0).left(failed(TestError)).or(pure(1)), "h", 6, "h", 1)
      }
    }

    "rule" >> {
      "single" >> {
        checkSuccess(char('h').or(char('w')), "h", 3, "", 'h')
        checkSuccess(char('h').or(char('w')), "hi", 3, "i", 'h')
        checkSuccess(failed(TestError).or(char('h')), "hi", 4, "i", 'h')
      }

      "with stack" >> {
        checkSuccess(char('h').left(char('i')).or(char('w')), "hi", 8, "", 'h')
        checkSuccess(char('h').left(char('i')).or(char('w')), "hi!", 8, "!", 'h')
        checkSuccess(char('h').left(failed(TestError)).or(char('h')), "hi", 8, "i", 'h')
      }
    }

    "multiple" >> {
      checkSuccess(char('h').or(char('w')).or(char('n')), "h", 4, "", 'h')
      checkSuccess(char('h').or(char('w')).or(char('n')), "w", 6, "", 'w')
      checkSuccess(char('h').or(char('w')).or(char('n')), "n", 8, "", 'n')

      checkSuccess(char('h').right(char('i')).or(char('w')).or(char('n')), "hi", 7, "", 'i')
      checkSuccess(char('h').right(char('i').or(char('w'))).or(char('n')), "hi", 7, "", 'i')
    }
  }

  "mix" >> {
    val parser =
      for {
        _ <- char('h')
        _ <- char('e').or(char('a')).or(char('o').right(char('l')).right(char('a')))
        _ <- char('l')
        _ <- char('l')
        _ <- char('o')
      } yield "world"

    checkSuccess(parser, "hello", 18, "", "world")
    checkSuccess(parser, "hallo", 20, "", "world")
    checkSuccess(parser, "holallo", 28, "", "world")
    checkFailure(parser, "hiho", 14, "iho", FailedParserInternal("not equals to 'o'", ParserState.Failed(FailedCondition(Vector('i')))))
  }

  case object TestError extends ParserState.ParserError

  private def checkSuccess[A](parser: Parser[A],
                              input: String,
                              expectedCycles: Int,
                              expectedRemainingInput: String,
                              expectedResult: A): MatchResult[Any] = {
    val (cycles, remaining, state) = ParserRuntime.runUnsafe(parser.asInstanceOf[Parser[Any]], input.toVector)

    cycles    mustEqual expectedCycles
    remaining mustEqual expectedRemainingInput.toVector
    state     mustEqual ParserState.Success(expectedResult)
  }

  private def checkFailure[A](parser: Parser[A],
                              input: String,
                              expectedCycles: Int,
                              expectedRemainingInput: String,
                              expectedError: ParserState.ParserError): MatchResult[Any] = {
    val (cycles, remaining, state) = ParserRuntime.runUnsafe(parser.asInstanceOf[Parser[Any]], input.toVector)

    cycles    mustEqual expectedCycles
    remaining mustEqual expectedRemainingInput.toVector
    state     mustEqual ParserState.Failed(expectedError)
  }
}
