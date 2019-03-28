package onegraph.parser

import onegraph.parser.internal.ParserState
import onegraph.parser.util.NonEmptySeq

sealed trait Parser[A] {

  def map[B](f: A => B): Parser[B] =
    Parser.FlatMap[A, B](this, a => Parser.Pure(ParserState.Success(f(a))))

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
}

object Parser {

  final case class Pure[A](a: ParserState[A]) extends Parser[A]

  final case class Rule[A](parse: Char => ParserState[A]) extends Parser[A]

  final case class Or[A](left: Parser[A], right: Parser[A]) extends Parser[A]

  final case class FlatMap[A, B](fa: Parser[A], f: A => Parser[B]) extends Parser[B]

  def pure[A](a: A): Parser[A] = Pure(ParserState.Success(a))

  def failed[A](error: ParserState.ParserError): Parser[A] = Pure(ParserState.Failed(error))

  def head: Parser[Char] = Rule(c => ParserState.Success(c))

  final case class FailedCondition(msg: String) extends ParserState.ParserError

  def satisfies(cond: Char => Boolean, operation: String): Parser[Char] =
    Rule(c => if (cond(c)) ParserState.Success(c) else ParserState.Failed(FailedCondition(s"'$c' $operation")))

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

  def char(c: Char): Parser[Char] =
    satisfies(_ == c, s"equal to '$c'")

  def digit: Parser[Char] =
    satisfies(c => '0' <= c && c <= '9', "is a digit")

  def natural: Parser[Int] =
    many(digit).map(_.foldLeft(0)((nat, d) => nat * 10 + (d - '0')))

  def int: Parser[Int] = (char('-').right(natural.map(-_))).or(natural)
}
