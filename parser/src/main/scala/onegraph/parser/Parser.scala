package onegraph.parser

import onegraph.parser.Parser.{CharStreamNotEmpty, WithErrorMessage}
import onegraph.parser.internal.{ParserResult, ParserRuntime}
import onegraph.parser.util.{NonEmptySeq, NumberHelper}

sealed trait Parser[A] {

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

  def combine(fa: Parser[A], f: (A, A) => A): Parser[A] =
    for {
      x <- this
      y <- fa
    } yield f(x, y)

  def or(fa: Parser[A]): Parser[A] = Parser.Or(this, fa)

  def |(fa: Parser[A]): Parser[A] = or(fa)

  def withError(msg: => String): Parser[A] = WithErrorMessage(this, () => msg)

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

  final case class Pure[A](a: ParserResult[A]) extends Parser[A]

  final case class Rule[A](arraySize: Int, parse: CharStream => ParserResult[A]) extends Parser[A]

  final case class Or[A](left: Parser[A], right: Parser[A]) extends Parser[A]

  final case class FlatMap[A, B](fa: Parser[A], f: A => Parser[B]) extends Parser[B]

  final case class WithErrorMessage[A](fa: Parser[A], msg: () => String) extends Parser[A]

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

  val letter  = satisfies(_.isLetter).withError("not a letter")
  val letters = many(letter)

  val digit  = satisfies(_.isDigit).withError("not a digit")
  val digits = many(digit)

  def char(c: Char): Parser[Char] =
    satisfies(_ == c).withError(s"not equals to '$c'")

  def oneOfChar(chars: Char*): Parser[Char] = {
    val charSet = chars.toSet

    satisfies(charSet.contains).withError(s"not in ${charSet.mkString("[", ", ", "]")}")
  }

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

  val positiveFloat =
    for {
      nat      <- natural
      floating <- maybe(char('.').right(natural))
      _        <- maybe(oneOfChar('f', 'F'))
    } yield {
      nat + {
        floating.fold(0f) { fl =>
          if (fl == 0)
            0f
          else
            fl / Math.pow(10d, Math.log10(fl.toDouble).toInt.toDouble + 1d).toFloat
        }
      }
    }

  val float = negativeNumber(positiveFloat)

  private implicit val doubleNumber = new NumberHelper[Double] {
    val zero = 0d

    def multByTen(x: Double): Double = x * 10.0d

    def addWithInt(x: Double, y: Int): Double = x + y

    def negate(x: Double): Double = -x
  }

  val positiveDouble =
    for {
      nat      <- natural
      floating <- maybe(char('.').right(natural))
      _        <- maybe(oneOfChar('d', 'D', 'f', 'F'))  
    } yield {
      nat + {
        floating.fold(0d) { fl =>
          if (fl == 0)
            0d
          else
            fl / Math.pow(10d, Math.log10(fl.toDouble).toInt.toDouble + 1d)
        }
      }
    }

  val double = negativeNumber(positiveDouble)

  // Token

  def literal(lit: String): Parser[String] =
    literalRaw(lit).map(_.mkString(""))

  def literalRaw(lit: String): Parser[CharStream] = {
    val chars = lit.toCharArray

    satisfies(lit.length, _.sameElements(chars)).withError(s"equal to ${chars.mkString("\"", "", "\"")}")
  }

  val word = letters.left(whitespaces).map(_.mkString(""))

  val comma = skip(char(','))

  def commaSep[A](parser: Parser[A]): Parser[Seq[A]] = {
    val as = for {
      a  <- parser
      _  <- comma.right(whitespaces)
      as <- commaSep(parser)
    } yield a +: as

    as.or(pure(Seq.empty))
  }
}
