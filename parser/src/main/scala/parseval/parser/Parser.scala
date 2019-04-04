package parseval.parser

import parseval.parser.Parser.CharStreamNotEmpty
import parseval.parser.internal.ParserRuntime
import parseval.parser.util.{NonEmptySeq, NumberHelper}

sealed trait Parser[+A] {

  def withError(msg: => String): Parser[A]

  def map[B](f: A => B): Parser[B] =
    Parser.FlatMap[A, B](this, a => Parser.Pure(ParserResult.Success(f(a))))

  def flatMap[B](f: A => Parser[B]): Parser[B] =
    Parser.FlatMap(this, f)

  def zip[B](fb: Parser[B]): Parser[(A, B)] =
    for {
      a <- this
      b <- fb
    } yield (a, b)

  def left[B](fb: Parser[B]): Parser[A] =
    this.flatMap(a => fb.map(_ => a))

  def right[B](fb: Parser[B]): Parser[B] =
    this.flatMap(_ => fb)

  def combine[A0 >: A](fa: Parser[A0], f: (A0, A0) => A0): Parser[A0] =
    for {
      x <- this
      y <- fa
    } yield f(x, y)

  def or[A0 >: A](fa: Parser[A0]): Parser[A0] = Parser.Or(this, fa)

  def |[A0 >: A](fa: Parser[A0]): Parser[A0] = or(fa)

  def apply(stream: CharStream): (CharStream, ParserResult[A]) =
    eval(stream)

  def eval(stream: CharStream): (CharStream, ParserResult[A]) =
    ParserRuntime.run(this, stream)

  def evalConsumeStream(stream: CharStream): ParserResult[A] = {
    val (remaining, result) = ParserRuntime.run(this, stream)

    if (remaining.isEmpty)
      result
    else
      ParserResult.Failed(CharStreamNotEmpty(remaining, result))
  }
}

object Parser {

  final case class CharStreamNotEmpty[A](remaining: CharStream, result: ParserResult[A]) extends ParserError

  final case class Pure[A](a: ParserResult[A]) extends Parser[A] {
    override def withError(msg: => String): Parser[A] = a match {
      case success@ParserResult.Success(_)                      => Pure(success)
      case ParserResult.Failed(FailedParserWithStack(error, stack)) => Pure(ParserResult.Failed(FailedParserWithStack(error, msg +: stack)))
      case ParserResult.Failed(error)                           => Pure(ParserResult.Failed(FailedParserWithStack(error, Vector(msg))))
    }
  }

  final case class Rule[A](arraySize: Int, parse: CharStream => ParserResult[A], errorMsg: Option[() => ErrorMsg] = None) extends Parser[A] {
    override def withError(msg: => String): Parser[A] = copy(errorMsg = Some(() => msg))
  }

  final case class Or[A](left: Parser[A], right: Parser[A], errorMsg: Option[() => ErrorMsg] = None) extends Parser[A] {
    override def withError(msg: => String): Parser[A] = copy(errorMsg = Some(() => msg))
  }

  final case class FlatMap[A, B](fa: Parser[A], f: A => Parser[B], errorMsg: Option[() => ErrorMsg] = None) extends Parser[B] {
    override def withError(msg: => String): Parser[B] = copy(errorMsg = Some(() => msg))
  }

  def pure[A](a: A): Parser[A] = Pure(ParserResult.Success(a))

  def failed[A](error: ParserError): Parser[A] = Pure(ParserResult.Failed(error))

  // Primitives

  final case class FailedCondition(input: CharStream) extends ParserError

  def satisfies(cond: Char => Boolean): Parser[Char] = {
    Rule(1, c => {
      if (cond(c.head))
        ParserResult.Success(c.head)
      else
        ParserResult.Failed(FailedCondition(c))
    })
  }

  def satisfies(arraySize: Int, cond: CharStream => Boolean): Parser[CharStream] = {
    Rule(arraySize, c => {
      if (cond(c))
        ParserResult.Success(c)
      else
        ParserResult.Failed(FailedCondition(c))
    })
  }

  def bracket[A, B, C](open: Parser[A], parser: Parser[B], close: Parser[C]): Parser[B] =
    open.right(parser).left(close)

  def many[A](parser: Parser[A]): Parser[Seq[A]] = {
    val as = for {
      a  <- parser
      as <- many(parser).or(pure(Seq.empty))
    } yield a +: as

    as.or(pure(Seq.empty))
  }

  def atLeastOnce[A](parser: Parser[A]): Parser[NonEmptySeq[A]] =
    for {
      a  <- parser
      as <- many(parser)
    } yield NonEmptySeq(a, as)

  def skip[A](parser: Parser[A]): Parser[Unit] =
    parser.map(_ => ())

  def skipMany[A](parser: Parser[A]): Parser[Unit] =
    many(skip(parser)).map(_ => ())

  def oneOf[A](parsers: Parser[A]*): Parser[A] =
    parsers.reduceLeft(_.or(_))

  def maybe[A](parser: Parser[A]): Parser[Option[A]] =
    parser.map[Option[A]](Some(_)).or(pure(None))

  // Chars

  private val spaceParser = satisfies(_.isSpaceChar).withError("not a space")
  val space  = skip(spaceParser)
  val spaces = skipMany(spaceParser)

  private val whitespaceParser = satisfies(_.isWhitespace).withError("not a whitespace")
  val whitespace  = skip(whitespaceParser)
  val whitespaces = skipMany(whitespaceParser)

  val control = satisfies(_.isControl).withError("not a control character")

  val letter  = satisfies(_.isLetter).withError("not a letter")
  val letters = many(letter)

  val digit  = satisfies(_.isDigit).withError("not a digit")
  val digits = many(digit)

  private val hexCharacters = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'F')

  val hexDigit  = satisfies(hexCharacters)
  val hexDigits = many(hexDigit)

  def char(c: Char): Parser[Char] =
    satisfies(_ == c).withError(s"not equals to '$c'")

  def oneOfChar(chars: Char*): Parser[Char] = {
    val charSet = chars.toSet

    satisfies(charSet.contains).withError(s"not in ${charSet.mkString("[", ", ", "]")}")
  }

  def literal(lit: String): Parser[String] =
    literalRaw(lit).map(_.mkString(""))

  def literalRaw(lit: String): Parser[CharStream] = {
    val chars = lit.toCharArray

    satisfies(lit.length, _.sameElements(chars)).withError(s"not equal to ${chars.mkString("\"", "", "\"")}")
  }

  val doubleQuote    = char('\"')
  val reverseSolidus = char('\\')
  val backspace      = char('\b')
  val formFeed       = char('\f')
  val newline        = char('\n')
  val carriageReturn = char('\r')
  val tab            = char('\t')

  // Number

  def positiveNumber[A: NumberHelper]: Parser[A] = {
    val num = NumberHelper[A]

    atLeastOnce(digit).map(_.toSeq.foldLeft(num.zero)((nat, d) => num.addWithInt(num.multByTen(nat), d - '0')))
  }

  def negativeNumber[A: NumberHelper](parser: Parser[A]): Parser[A] =
    char('-').right(parser.map(NumberHelper[A].negate)) | parser

  private implicit val naturalNumber = new NumberHelper[Int] {
    val zero = 0

    def multByTen(x: Int): Int = x * 10

    def addWithInt(x: Int, y: Int): Int = x + y

    def negate(x: Int): Int = -x
  }

  val natural     = positiveNumber[Int]
  val positiveInt = natural
  val int         = negativeNumber(natural)

  private implicit val longNumber = new NumberHelper[Long] {
    val zero = 0l

    def multByTen(x: Long): Long = x * 10

    def addWithInt(x: Long, y: Int): Long = x + y

    def negate(x: Long): Long = -x
  }

  val positiveLong =
    for {
      l <- positiveNumber[Long]
      _ <- maybe(oneOfChar('l', 'L'))
    } yield l

  val long = negativeNumber(positiveLong)

  private implicit val floatNumber = new NumberHelper[Float] {
    val zero = 0f

    def multByTen(x: Float): Float = x * 10.0f

    def addWithInt(x: Float, y: Int): Float = x + y

    def negate(x: Float): Float = -x
  }

  private def scientific(base: Int) =
    oneOfChar('e' , 'E').right(int).map(exp => base.toDouble * Math.pow(10d, exp.toDouble))

  private def floating(base: Int) =
    char('.').right(natural).map { fl =>
      base.toDouble + {
        if (fl == 0)
          0d
        else
          fl / Math.pow(10d, Math.log10(fl.toDouble).toInt.toDouble + 1d)
      }
    }

  val positiveFloat =
    for {
      base <- natural
      flt  <- maybe(floating(base) | scientific(base))
      _    <- maybe(oneOfChar('f', 'F'))
    } yield flt.fold(base.toFloat)(_.toFloat)

  val float = negativeNumber(positiveFloat)

  private implicit val doubleNumber = new NumberHelper[Double] {
    val zero = 0d

    def multByTen(x: Double): Double = x * 10.0d

    def addWithInt(x: Double, y: Int): Double = x + y

    def negate(x: Double): Double = -x
  }

  val positiveDouble =
    for {
      base <- natural
      dbl  <- maybe(floating(base) | scientific(base))
      _    <- maybe(oneOfChar('d', 'D', 'f', 'F'))
    } yield dbl.fold(base.toDouble)(identity)

  val double = negativeNumber(positiveDouble)

  // Token

  def lexeme[A](parser: Parser[A]) = parser.left(whitespaces)

  val stringRaw = many(
    letter
      | digit
      | doubleQuote
      | reverseSolidus
      | backspace
      | formFeed
      | newline
      | carriageReturn
      | tab
      | control
  )

  val string = stringRaw.map(_.mkString(""))

  val comma = skip(char(','))

  def commaSep[A](parser: Parser[A]): Parser[Seq[A]] = {
    val as = for {
      a  <- parser
      _  <- lexeme(comma)
      as <- commaSep(parser)
    } yield a +: as

    as.or(pure(Seq.empty))
  }
}
