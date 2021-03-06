package parseval.parser.internal

import parseval.parser._

import scala.collection.mutable
import scala.util.control.NonFatal

object ParserRuntime {

  import Parser._

  private type UnsafeFlatMap = Any => Parser[Any]
  private type IsBranch      = Boolean
  private type LazyErrorMsg  = () => ErrorMsg
  private type StackFrame    = (IsBranch, UnsafeFlatMap)

  final case class InternalParserError(cause: Throwable) extends ParserError

  private[internal] final class RuntimeState(private var current: Parser[Any],
                                             private var stream: CharStream) {

    private val stack          = mutable.ArrayStack[StackFrame]()
    private val streamPointers = mutable.ArrayStack[(CharStream, Vector[LazyErrorMsg])]()

    private var errorMsgStack = Vector.empty[LazyErrorMsg]

    private var orBranches = 0
    private var cycleCount = 0

    private[internal] def popStack(): StackFrame =
      if (stack.nonEmpty)
        stack.pop()
      else
        null

    def stackFramesLeft: Boolean = stack.nonEmpty

    def pushStackFrame(f: Nothing => Parser[Any]): Unit =
      stack.push((false, f.asInstanceOf[UnsafeFlatMap]))

    def popStackFrame(): UnsafeFlatMap = {
      var frame = popStack()

      while (frame != null && frame._1) {
        frame       = popStack()
        orBranches -= 1

        streamPointers.pop()
      }

      if (frame != null)
        frame._2
      else
        null
    }

    def orBranchesLeft: Boolean = orBranches > 0

    def pushOrBranch(left: Parser[Any], right: Parser[Any], errorMsg: Option[LazyErrorMsg]): Unit = {
      setCurrent(left)
      pushErrorMsg(errorMsg)

      orBranches += 1

      stack.push((true, _ => right))
      streamPointers.push((stream, errorMsgStack))
    }

    def popStackUntilOrBranch(): Parser[Any] = {
      var orBranch = popStack()

      while (!orBranch._1) {
        orBranch = popStack()
      }

      val (resetStream, resetErrorMsg) = streamPointers.pop()

      stream         = resetStream
      errorMsgStack  = resetErrorMsg
      orBranches    -= 1

      orBranch._2(())
    }

    def pushErrorMsg(msg: Option[LazyErrorMsg]): Unit =
      msg.foreach(m => errorMsgStack = m +: errorMsgStack)

    def enoughStreamChars(readCount: Int): Boolean = stream.length >= readCount

    def remainingCharsCount: Int = stream.length

    def splitStreamChars(readCount: Int): (CharStream, CharStream) = stream.splitAt(readCount)

    def failedResultFromErrorMsg(cause: ParserResult.Failed): ParserResult[Any] =
      if (errorMsgStack.isEmpty)
        cause
      else
        ParserResult.Failed(FailedParserWithStack(cause.error, errorMsgStack.map(_())))

    def incrCycleCount = cycleCount += 1

    def produceResult(result: ParserResult[Any]): (Int, CharStream, ParserResult[Any]) =
      (cycleCount, stream, result)

    def getCurrent: Parser[Any] = current

    def setCurrent(next: Parser[Any]): Unit = current = next

    def getErrorMsgStack: Vector[LazyErrorMsg] = errorMsgStack

    def setStream(remaining: CharStream): Unit = stream = remaining
  }

  private[internal] def runUnsafe(parser: Parser[Any], inputStream: CharStream): (Int, CharStream, ParserResult[Any]) = {
    val state = new RuntimeState(parser, inputStream)

    var finalResult: ParserResult[Any] = null

    while (finalResult == null || (!finalResult.isSuccess && state.orBranchesLeft)) {
      state.getCurrent match {
        case Pure(value) =>
          if (state.stackFramesLeft && value.isSuccess) {
            val frame = state.popStackFrame()

            if (frame != null) {
              state.setCurrent(frame(value.get))
            }
            else {
              finalResult = value
            }
          }
          else if (state.stackFramesLeft && state.orBranchesLeft) {
            state.setCurrent(state.popStackUntilOrBranch())
          }
          else if (value.isSuccess) {
            finalResult = value
          }
          else {
            finalResult = state.failedResultFromErrorMsg(value.asInstanceOf[ParserResult.Failed])
          }

        case Rule(readCount, parse, errorMsg) =>
          if (state.enoughStreamChars(readCount)) {
            val (toProcess, remaining) = state.splitStreamChars(readCount)
            val result                 = parse(toProcess)

            if (state.stackFramesLeft && result.isSuccess) {
              val frame = state.popStackFrame()

              if (frame != null)
                state.setCurrent(frame(result.get))
              else
                finalResult = result

              state.setStream(remaining)
            }
            else if (!state.stackFramesLeft && result.isSuccess) {
              state.setStream(remaining)
              finalResult = result
            }
            else if (state.orBranchesLeft && !result.isSuccess) {
              state.setCurrent(state.popStackUntilOrBranch())
            }
            else {
              // TODO add line number, column information
              state.pushErrorMsg(errorMsg)

              finalResult = state.failedResultFromErrorMsg(result.asInstanceOf[ParserResult.Failed])
            }
          }
          else if (state.orBranchesLeft) {
            state.setCurrent(state.popStackUntilOrBranch())
          }
          else {
            state.pushErrorMsg(errorMsg)

            finalResult = ParserResult.Failed(NotEnoughCharacters(state.remainingCharsCount, readCount))
          }

        case Or(left, right, errorMsg) =>
          state.pushOrBranch(left, right, errorMsg)

        case FlatMap(fa, f, errorMsg) =>
          state.setCurrent(fa)
          state.pushStackFrame(f)
          state.pushErrorMsg(errorMsg)
      }

      state.incrCycleCount
    }

    state.produceResult(finalResult)
  }

  def run[A](parser: Parser[A], stream: CharStream): (CharStream, ParserResult[A]) =
    try {
      val (_, remaining, result) = runUnsafe(parser.asInstanceOf[Parser[Any]], stream)

      (remaining, result.asInstanceOf[ParserResult[A]])
    } catch {
      case NonFatal(cause) => (stream, ParserResult.Failed(InternalParserError(cause)))
    }
}
