package onegraph.parser.internal

import onegraph.parser.{CharStream, Parser}

import scala.collection.mutable
import scala.util.control.NonFatal

object ParserRuntime {

  import Parser._

  private type UnsafeFlatMap = Any => Parser[Any]
  private type IsBranch      = Boolean

  private[internal] def runUnsafe(parser: Parser[Any], inputStream: CharStream): (Int, CharStream, ParserState[Any]) = {
    val stack       = mutable.ArrayStack[(IsBranch, UnsafeFlatMap)]()
    val streamStack = mutable.ArrayStack[CharStream]()

    var current    = parser
    var stream     = inputStream
    var orBranches = 0
    var cycleCount = 0

    var result:ParserState[Any] = null

    def stackPop(): (IsBranch, UnsafeFlatMap) = {
      if (stack.nonEmpty)
        stack.pop()
      else
        null
    }

    def popStackUntilOrBranch(): Parser[Any] = {
      var orBranch = stackPop()

      while (!orBranch._1) {
        orBranch = stackPop()
      }

      orBranches -= 1
      stream      = streamStack.pop()
      orBranch._2(())
    }

    def popStackFrame(): UnsafeFlatMap = {
      var frame = stackPop()

      while (frame != null && frame._1) {
        frame       = stackPop()
        orBranches -= 1

        streamStack.pop()
      }

      if (frame != null)
        frame._2
      else
        null
    }

    while (result == null || (!result.isSuccess && orBranches > 0)) {
      current match {
        case Pure(state) =>
          if (stack.nonEmpty && state.isSuccess) {
            val frame = popStackFrame()

            if (frame != null)
              current = frame(state.get)
            else
              result = state
          }
          else if (stack.nonEmpty && orBranches > 0) {
            current = popStackUntilOrBranch()
          }
          else {
            result = state
          }

        case Rule(parse) =>
          if (stream.nonEmpty) {
            val state = parse(stream.head)

            if (stack.nonEmpty && state.isSuccess) {
              val frame = popStackFrame()

              if (frame != null)
                current = frame(state.get)
              else
                result = state

              stream  = stream.tail
            }
            else if (stack.isEmpty && state.isSuccess) {
              stream = stream.tail
              result = state
            }
            else if (stack.nonEmpty && orBranches > 0) {
              current = popStackUntilOrBranch()
            }
            else {
              // TODO add line number, column information
              result = state
            }
          }
          else {
            result = ParserState.Failed(NoCharactersLeft)
          }

        case Or(left, right) =>
          current = left

          stack.push(true -> (_ => right))
          streamStack.push(stream)
          orBranches += 1

        case FlatMap(fa, f) =>
          current = fa

          stack.push(false -> f.asInstanceOf[UnsafeFlatMap])
      }

      cycleCount += 1
    }

    (cycleCount, stream, result)
  }

  def run[A](parser: Parser[A], stream: CharStream): ParserState[A] =
    try {
      runUnsafe(parser.asInstanceOf[Parser[Any]], stream)._3.asInstanceOf[ParserState[A]]
    } catch {
      case NonFatal(cause) => ParserState.Failed(InternalParserError(cause))
    }
}

sealed trait ParseRuntimeError extends ParserState.ParserError

case object NoCharactersLeft extends ParseRuntimeError

final case class InternalParserError(cause: Throwable) extends ParseRuntimeError
