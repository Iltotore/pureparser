package io.github.iltotore.pureparser

import purelogic.*
import scala.util.matching.Regex
import io.github.iltotore.pureparser.util.ByName
import io.github.iltotore.pureparser.util.Zip
import scala.annotation.tailrec
import scala.annotation.nowarn
import scala.compiletime.erasedValue

type Parser[-I, +A] = (State[Int], Reader[List[I]], Writer[ParseError], Abort[Unit]) ?=> A

object Parser:

  def apply[I, A](input: List[I])(parser: Parser[I, A]): ParseResult[A] =
    val (errors, output) = Logic.run(state = 0, reader = input)(parser)
    val outputOpt = output.toOption
    ParseResult(outputOpt.map(_._2), errors, outputOpt.fold(0)(_._1))

  def apply[A](input: String)(parser: Parser[Char, A]): ParseResult[A] =
    apply(input.toList)(parser)

  def abort: Parser[Any, Nothing] = fail(())

  def errorAndAbort(error: ParseError): Parser[Any, Nothing] =
    write(error)
    abort

  def isEOF: Parser[Any, Boolean] = read.sizeCompare(get) <= 0

  def peek[I]: Parser[I, I] =
    val input = read
    val next = get
    if next < input.size then input(next)
    else errorAndAbort(ParseError.EOF)

  def advance(n: Int): Parser[Any, Unit] = update(_ + n)

  def next[I]: Parser[I, I] =
    val result = peek
    advance(1)
    result

  def remaining[I]: Parser[I, List[I]] = read.drop(get)

  def eof[I]: Parser[I, Unit] =
    if Parser.isEOF then ()
    else errorAndAbort(ParseError.UnexpectedToken("End of file", get))

  def debug[I, A](parser: Parser[I, A], name: String): Parser[I, A] =
    val start =
      if Parser.isEOF then "<EOF>"
      else Parser.peek.toString
    println(s"Starting $name at $get. Next token: $start")
    val result = parser

    val end =
      if Parser.isEOF then "<EOF>"
      else Parser.peek.toString
    println(s"Ending $name at $get. Next token: $end. Result: $result")
    result

  def span[I, A](parser: Parser[I, A])(using zip: Zip[A, Span]): Parser[I, zip.Zipped] =
    val start = get
    val result = parser
    zip.zip(result, Span(start, get))

  def literal[I](value: I)(using CanEqual[I, I]): Parser[I, Unit] =
    val start = get
    if next == value then ()
    else errorAndAbort(ParseError.UnexpectedToken(value.toString, start))

  def literal(value: String): Parser[Char, Unit] =
    var i = 0
    val start = get

    while i < value.size do
      if next != value(i) then errorAndAbort(ParseError.UnexpectedToken(value, start))
      i += 1

  def oneOf[I](values: I*): Parser[I, I] =
    val start = get
    val result = next
    if values.contains(result) then result
    else errorAndAbort(ParseError.UnexpectedToken(values.mkString("One of: ", ", ", ""), start))

  def oneOf(values: String): Parser[Char, Char] =
    oneOf[Char](values*)

  def regex(pattern: Regex): Parser[Char, String] =
    pattern.findPrefixOf(remaining.mkString) match
      case Some(value) =>
        advance(value.length)
        value
      case None => errorAndAbort(ParseError.UnexpectedToken(s"Text matching regex $pattern", get))
    
  def regex(pattern: String): Parser[Char, String] = regex(pattern.r)

  def whitespace: Parser[Char, Unit] =
    val start = get
    if next.isWhitespace then ()
    else Parser.errorAndAbort(ParseError.UnexpectedToken("Whitespace", start))

  def spaced[A](parser: Parser[Char, A]): Parser[Char, A] =
    Parser.repeatDiscard0(Parser.whitespace, "prefix")
    val result = parser
    Parser.repeatDiscard0(Parser.whitespace)
    result

  @nowarn("msg=unused")
  def as[I, A](parser: Parser[I, Any], value: A): Parser[I, A] =
    parser
    value

  inline def unit[I](inline parser: Parser[I, Any]): Parser[I, Unit] = Parser.as(parser, ())

  def firstOfSeq[I, A](parsers: Seq[ByName ?=> Parser[I, A]]): Parser[I, A] = parsers match
    case head +: tail => recover(head(using ByName))(_ => firstOfSeq(tail))
    case _ => fail(())

  def firstOf[I, A](parsers: (ByName ?=> Parser[I, A])*): Parser[I, A] =
    firstOfSeq(parsers)

  inline def inOrder[I, T <: Tuple](parsers: T): Parser[I, Zip.All[T]] =
    val parserList = parsers.toList.asInstanceOf[List[Any]]
    val zips = Zip.foldingAll[T]
    parserList
      .zip(zips)
      .foldRight[Any](()):
        case ((value, zip), tail) => zip.zip(value, tail)
      .asInstanceOf[Zip.All[T]]

  def isSuccessful[I](parser: Parser[I, Any]): Parser[I, Boolean] =
    localState(identity)(recover(Parser.as(parser, true))(_ => false))

  def orError[I, A](parser: Parser[I, A], error: ParseError): Parser[I, A] =
    recover(parser)(_ => Parser.errorAndAbort(error))

  def expect[I, A](parser: Parser[I, A], expected: String): Parser[I, A] =
    val start = get
    Parser.orError(parser, ParseError.UnexpectedToken(expected, start))

  def not[I, A](parser: Parser[I, A]): Parser[I, Unit] =
    if Parser.isSuccessful(parser) then
      Parser.errorAndAbort(ParseError.UnexpectedToken("Input not validating this parser", get))
    else ()

  def andCheck[I, A](parser: Parser[I, A], check: Parser[I, Unit]): Parser[I, A] =
    if Parser.isSuccessful(check) then parser
    else Parser.errorAndAbort(ParseError.UnexpectedToken("Something else", get))

  def recoverWith[I, A](parser: Parser[I, A], strategy: RecoverStrategy[I, A]): Parser[I, A] =
    recoverKeepLog(parser)(_ => Writer((strategy(parser)))._2)

  @tailrec
  def skipUntil[I](until: Parser[I, Any]): Parser[I, Unit] =
    if Parser.isSuccessful(until) then ()
    else if Parser.isEOF then errorAndAbort(ParseError.EOF)
    else
      advance(1)
      skipUntil(until)

  def repeatUntil[I, A](parser: Parser[I, A], until: Parser[I, Unit]): Parser[I, List[A]] =

    @tailrec
    def rec(accumulator: List[A]): Parser[I, List[A]] =
      val head = parser
      if Parser.isSuccessful(until) then accumulator :+ head
      else rec(accumulator :+ head)

    rec(Nil)

  def repeatUntil0[I, A](parser: Parser[I, A], until: Parser[I, Unit]): Parser[I, List[A]] =
    if Parser.isSuccessful(until) then Nil
    else Parser.repeatUntil(parser, until)

  @tailrec
  def repeatDiscard0[I](parser: Parser[I, Any]): Parser[I, Unit] =
    if Parser.isEOF || !Parser.isSuccessful(parser) then ()
    else
      Parser.unit(parser)
      Parser.repeatDiscard0(parser)

  def separatedByUntil[I, A](parser: Parser[I, A], separator: Parser[I, Unit], until: Parser[I, Unit]): Parser[I, List[A]] =

    @tailrec
    def rec(accumulator: List[A]): Parser[I, List[A]] =
      if Parser.isSuccessful(until) then accumulator
      else
        separator
        val head = parser
        rec(accumulator :+ head)

    if Parser.isSuccessful(until) then Nil
    else
      val head = parser
      head :: rec(Nil)

  def separatedByReduce[I, A](parser: Parser[I, A], separator: Parser[I, (A, A) => A]): Parser[I, A] =

    @tailrec
    def rec(accumulator: A): Parser[I, A] =
      if Parser.isSuccessful(separator) then rec(separator(accumulator, parser))
      else accumulator

    rec(parser)