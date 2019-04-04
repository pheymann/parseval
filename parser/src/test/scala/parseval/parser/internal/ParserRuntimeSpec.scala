package parseval.parser.internal

import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import parseval.parser._
import parseval.parser.internal.ParserRuntime.RuntimeState

object ParserRuntimeSpec extends Specification {

  import Parser._

  sequential

  "RuntimeState" >> {
    val parser1 = char('h').asInstanceOf[Parser[Any]]
    val parser2 = char('n').asInstanceOf[Parser[Any]]
    val frame1  = (_: Any) => parser1
    val frame2  = (_: Any) => parser2

    "push/pop stack frames" >> {
      val state = new RuntimeState(null, null)

      state.pushStackFrame(frame1)
      state.pushStackFrame(frame2)

      state.stackFramesLeft must beTrue
      state.popStack() mustEqual (false, frame2)
      state.stackFramesLeft must beTrue
      state.popStack() mustEqual (false, frame1)
      state.stackFramesLeft must beFalse
    }

    "push/pop OR branches" >> {
      val state = new RuntimeState(null, null)

      state.orBranchesLeft must beFalse
      state.pushOrBranch(parser1, parser2, None)
      state.orBranchesLeft must beTrue
      state.getCurrent mustEqual parser1

      state.popStackUntilOrBranch() mustEqual parser2
      state.orBranchesLeft must beFalse

      val errorMsg1 = () => "hi"
      val errorMsg2 = () => "ho"

      state.pushOrBranch(parser1, parser2, Some(errorMsg1))
      state.pushStackFrame(frame1)
      state.pushErrorMsg(Some(errorMsg2))
      state.pushErrorMsg(None)
      state.getErrorMsgStack mustEqual Vector(errorMsg2, errorMsg1)

      state.popStackUntilOrBranch() mustEqual parser2
      state.getErrorMsgStack mustEqual Vector(errorMsg1)

      state.pushOrBranch(parser1, parser2, None)
      state.pushStackFrame(frame1)
      state.pushOrBranch(parser2, parser1, None)
      state.pushStackFrame(frame2)

      state.popStackUntilOrBranch() mustEqual parser1
      state.popStackUntilOrBranch() mustEqual parser2
      state.popStackUntilOrBranch() must throwA(new NullPointerException(null))

      state.pushStackFrame(frame1)
      state.pushOrBranch(parser1, parser2, None)
      state.pushStackFrame(frame2)

      state.popStackFrame() mustEqual frame2
      state.orBranchesLeft must beTrue
      state.popStackFrame() mustEqual frame1
      state.orBranchesLeft must beFalse
    }
  }

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
        checkSuccess(char('h'), "h", 1, "", 'h')
        checkSuccess(char('h'), "hi", 1, "i", 'h')
      }

      "with stack" >> {
        checkSuccess(char('h').right(char('i')).map(_ => 0), "hi", 5, "", 0)
        checkSuccess(char('h').right(char('i')).map(_ => 0), "hi!", 5, "!", 0)
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
        checkFailure(char('h'), "c", 1, "c", FailedParserWithStack(FailedCondition(Vector('c')), Vector("not equals to 'h'")))
        checkFailure(char('h'), "ci", 1, "ci", FailedParserWithStack(FailedCondition(Vector('c')), Vector("not equals to 'h'")))
        checkFailure(char('h'), "", 1, "", NotEnoughCharacters(0, 1))
      }

      "with stack" >> {
        checkFailure(char('h').right(char('i')).map(_ => 0), "ho", 4, "o", FailedParserWithStack(FailedCondition(Vector('o')), Vector("not equals to 'i'")))
        checkFailure(char('h').right(char('i')).map(_ => 0), "hola", 4, "ola", FailedParserWithStack(FailedCondition(Vector('o')), Vector("not equals to 'i'")))
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
        checkSuccess(char('h').or(char('w')), "h", 2, "", 'h')
        checkSuccess(char('h').or(char('w')), "hi", 2, "i", 'h')
        checkSuccess(failed(TestError).or(char('h')), "hi", 3, "i", 'h')
      }

      "with stack" >> {
        checkSuccess(char('h').left(char('i')).or(char('w')), "hi", 6, "", 'h')
        checkSuccess(char('h').left(char('i')).or(char('w')), "hi!", 6, "!", 'h')
        checkSuccess(char('h').left(failed(TestError)).or(char('h')), "hi", 6, "i", 'h')
        checkSuccess(char('h').flatMap(_ => char('i')).or(char('h')), "h", 5, "", 'h')
      }
    }

    "multiple" >> {
      checkSuccess(char('h').or(char('w')).or(char('n')), "h", 3, "", 'h')
      checkSuccess(char('h').or(char('w')).or(char('n')), "w", 4, "", 'w')
      checkSuccess(char('h').or(char('w')).or(char('n')), "n", 5, "", 'n')

      checkSuccess(char('h').right(char('i')).or(char('w')).or(char('n')), "hi", 5, "", 'i')
      checkSuccess(char('h').right(char('i').or(char('w'))).or(char('n')), "hi", 5, "", 'i')
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

    checkSuccess(parser, "hello", 13, "", "world")
    checkSuccess(parser, "hallo", 14, "", "world")
    checkSuccess(parser, "holallo", 19, "", "world")
    checkFailure(parser, "hiho", 10, "iho", FailedParserWithStack(FailedCondition(Vector('i')), Vector("not equals to 'o'")))
  }

  case object TestError extends ParserError

  private def checkSuccess[A](parser: Parser[A],
                              input: String,
                              expectedCycles: Int,
                              expectedRemainingInput: String,
                              expectedResult: A): MatchResult[Any] = {
    val (cycles, remaining, state) = ParserRuntime.runUnsafe(parser.asInstanceOf[Parser[Any]], input.toVector)

    cycles    mustEqual expectedCycles
    remaining mustEqual expectedRemainingInput.toVector
    state     mustEqual ParserResult.Success(expectedResult)
  }

  private def checkFailure[A](parser: Parser[A],
                              input: String,
                              expectedCycles: Int,
                              expectedRemainingInput: String,
                              expectedError: ParserError): MatchResult[Any] = {
    val (cycles, remaining, state) = ParserRuntime.runUnsafe(parser.asInstanceOf[Parser[Any]], input.toVector)

    cycles    mustEqual expectedCycles
    remaining mustEqual expectedRemainingInput.toVector
    state     mustEqual ParserResult.Failed(expectedError)
  }
}
