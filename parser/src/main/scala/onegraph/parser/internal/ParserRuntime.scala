package onegraph.parser.internal

import onegraph.parser.{CharStream, Parser}

import scala.collection.mutable
import scala.util.control.NonFatal

object ParserRuntime {

  import Parser._

  final case class FailedParserInternal(msg: String, cause: ParserState[Any]) extends ParserState.ParserError

  private type UnsafeFlatMap = Any => Parser[Any]
  private type IsBranch      = Boolean
  private type ErrorMsg      = Option[() => String]
  private type StackFrame    = (IsBranch, UnsafeFlatMap)

  private final class RuntimeState(var current: Parser[Any],
                                   var stream: CharStream) {

    val stack          = mutable.ArrayStack[StackFrame]()
    val streamPointers = mutable.ArrayStack[(CharStream, ErrorMsg)]()

    var errorMsg = Option.empty[() => String]

    var orBranches = 0
    var cycleCount = 0
  }

  private def popStack(state: RuntimeState): StackFrame =
    if (state.stack.nonEmpty)
      state.stack.pop()
    else
      null


  private def popStackUntilOrBranch(state: RuntimeState): Parser[Any] = {
    var orBranch = popStack(state)

    while (!orBranch._1) {
      orBranch = popStack(state)
    }

    val (stream, errorMsg) = state.streamPointers.pop()

    state.stream      = stream
    state.errorMsg    = errorMsg
    state.orBranches -= 1

    orBranch._2(())
  }

  private def popStackFrame(state: RuntimeState): UnsafeFlatMap = {
    var frame = popStack(state)

    while (frame != null && frame._1) {
      frame             = popStack(state)
      state.orBranches -= 1

      state.streamPointers.pop()
    }

    if (frame != null)
      frame._2
    else
      null
  }

  private def toFailedParser(errorMsg: Option[() => String], cause: ParserState[Any]): ParserState[Any] =
    errorMsg.fold(cause)(msg => ParserState.Failed(FailedParserInternal(msg(), cause)))

  private[internal] def runUnsafe(parser: Parser[Any], inputStream: CharStream): (Int, CharStream, ParserState[Any]) = {
    val state = new RuntimeState(parser, inputStream)

    var result: ParserState[Any] = null

    while (result == null || (!result.isSuccess && state.orBranches > 0)) {
      state.current match {
        case Pure(value) =>
          if (state.stack.nonEmpty && value.isSuccess) {
            val frame = popStackFrame(state)

            if (frame != null) {
              state.current = frame(value.get)
            }
            else {
              result = value
            }
          }
          else if (state.stack.nonEmpty && state.orBranches > 0) {
            state.current = popStackUntilOrBranch(state)
          }
          else if (value.isSuccess) {
            result = value
          }
          else {
            result = toFailedParser(state.errorMsg, value)
          }

        case Rule(readCount, parse) =>
          if (state.stream.length >= readCount) {
            val (toProcess, remaining) = state.stream.splitAt(readCount)
            val value                  = parse(toProcess)

            if (state.stack.nonEmpty && value.isSuccess) {
              val frame = popStackFrame(state)

              if (frame != null)
                state.current = frame(value.get)
              else
                result = value

              state.stream = remaining
            }
            else if (state.stack.isEmpty && value.isSuccess) {
              state.stream = remaining
              result       = value
            }
            else if (state.stack.nonEmpty && state.orBranches > 0) {
              state.current = popStackUntilOrBranch(state)
            }
            else {
              // TODO add line number, column information
              result = toFailedParser(state.errorMsg, value)
            }
          }
          else {
            result = ParserState.Failed(NotEnoughCharacters(state.stream.length, readCount))
          }

        case Or(left, right) =>
          state.current     = left
          state.orBranches += 1

          state.stack.push((true, _ => right))
          state.streamPointers.push((state.stream, state.errorMsg))

        case FlatMap(fa, f) =>
          state.current = fa

          state.stack.push((false, f.asInstanceOf[UnsafeFlatMap]))

        case WithErrorMessage(fa, errorMsg) =>
          state.current  = fa
          state.errorMsg = Some(errorMsg)
      }

      state.cycleCount += 1
    }

    (state.cycleCount, state.stream, result)
  }

  def run[A](parser: Parser[A], stream: CharStream): ParserState[A] =
    try {
      runUnsafe(parser.asInstanceOf[Parser[Any]], stream)._3.asInstanceOf[ParserState[A]]
    } catch {
      case NonFatal(cause) => ParserState.Failed(InternalParserError(cause))
    }
}

sealed trait ParseRuntimeError extends ParserState.ParserError

final case class NotEnoughCharacters(actual: Int, expected: Int) extends ParseRuntimeError

final case class InternalParserError(cause: Throwable) extends ParseRuntimeError
