package io.github.iltotore.pureparser

import purelogic.*
import scala.util.matching.Regex
import io.github.iltotore.pureparser.util.ByName
import io.github.iltotore.pureparser.util.Zip
import scala.annotation.tailrec
import scala.annotation.nowarn
import scala.compiletime.erasedValue

/**
  * A parser taking tokens of type [[I]], outputting a [[A]].
  * 
  * It is a combination of the following effects:
  * - [[State]] of [[Int]] representing the index of the next token to read.
  * - [[Reader]] of a [[List]] of tokens [[I]].
  * - [[Writer]] of [[ParseError]].
  * - [[Abort]] of [[Unit]], used to cut a path.
  * 
  * @tparam I the type of a token.
  * @tparam A the output type.
  */
type Parser[-I, +A] = (State[Int], Reader[List[I]], Writer[ParseError], Abort[Unit]) ?=> A

object Parser:

  /**
    * Run the given [[Parser]] on the given [[List]] of tokens.
    *
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param input a [[List]] of tokens to parse.
    * @param parser the [[Parser]] to run.
    * @return the result of the parsing process on the given input.
    */
  def apply[I, A](input: List[I])(parser: Parser[I, A]): ParseResult[A] =
    val (errors, output) = Logic.run(state = 0, reader = input)(parser)
    val outputOpt = output.toOption
    ParseResult(outputOpt.map(_._2), errors, outputOpt.fold(0)(_._1))

  /**
    * Run the given [[Parser]] on the given [[String]], treated as a list of [[Char]] tokens.
    *
    * @tparam A the output type.
    * @param input a [[List]] of tokens to parse.
    * @param parser the [[Parser]] to run.
    * @return the result of the parsing process on the given input.
    */
  def apply[A](input: String)(parser: Parser[Char, A]): ParseResult[A] =
    apply(input.toList)(parser)

  /**
    * Abort/Cut this branch.
    */
  def abort: Parser[Any, Nothing] = fail(())

  /**
    * Emit an error then [[abort]].
    *
    * @param error the error to emit.
    */
  def errorAndAbort(error: ParseError): Parser[Any, Nothing] =
    write(error)
    abort

  /**
    * Check if there is no more token to parse.
    */
  def isEOF: Parser[Any, Boolean] = read.sizeCompare(get) <= 0

  /**
    * Get the next character to parse without updating advancing.
    * 
    * @tparam I the type of a token.
    */
  def peek[I]: Parser[I, I] =
    val input = read
    val next = get
    if next < input.size then input(next)
    else errorAndAbort(ParseError.EOF)

  /**
    * Advance the cursor in the token list.
    *
    * @param n the number of tokens to skip.
    */
  def advance(n: Int): Parser[Any, Unit] = update(_ + n)

  /**
    * Read the next token. Like [[peek]] then [[advance]] of 1.
    * 
    * @tparam I the type of a token.
    */
  def next[I]: Parser[I, I] =
    val result = peek
    advance(1)
    result

  /**
    * Get all the remaining tokens to read.
    * 
    * @tparam I the type of a token.
    */
  def remaining[I]: Parser[I, List[I]] = read.drop(get)

  /**
    * Succeed only if there is no token left to read.
    * 
    * @tparam I the type of a token.
    */
  def eof[I]: Parser[I, Unit] =
    if Parser.isEOF then ()
    else errorAndAbort(ParseError.UnexpectedToken("End of file", get))

  /**
    * Debug the given [[Parser]] by printing info before and after parsing.
    *
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parser the [[Parser]] to debug.
    * @param name the name of the [[Parser]], used for debug information.
    */
  def debug[I, A](parser: Parser[I, A], name: String): Parser[I, A] =
    val start =
      if Parser.isEOF then "<EOF>"
      else Parser.peek.toString
    println(s"Starting $name at $get. Next token: $start")
    val (errors, result) = Writer.capture(parser)

    val end =
      if Parser.isEOF then "<EOF>"
      else Parser.peek.toString
    println(s"Ending $name at $get. Next token: $end. Result: $result. Errors: $errors")
    result

  /**
    * Return both the output of the given [[Parser]] and the [[Span]] between the first and last read token.
    *
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parser the [[Parser]] to wrap.
    */
  def span[I, A](parser: Parser[I, A])(using zip: Zip[A, Span]): Parser[I, zip.Zipped] =
    val start = get
    val result = parser
    zip.zip(result, Span(start, get))

  /**
    * Parse a specific token.
    *
    * @tparam I the type of a token.
    * @param value the expected token.
    */
  def literal[I](value: I)(using CanEqual[I, I]): Parser[I, Unit] =
    val start = get
    if next == value then ()
    else errorAndAbort(ParseError.UnexpectedToken(value.toString, start))

  /**
    * Parse a specific [[String]].
    *
    * @param value the expected text.
    */
  def literal(value: String): Parser[Char, Unit] =
    var i = 0
    val start = get

    while i < value.size do
      if next != value(i) then errorAndAbort(ParseError.UnexpectedToken(value, start))
      i += 1

  /**
    * Parse one of the expected tokens.
    * 
    * @tparam I the type of a token.
    * @param values the allowed tokens.
    */
  def oneOf[I](values: Set[I]): Parser[I, I] =
    val start = get
    val result = next
    if values.contains(result) then result
    else errorAndAbort(ParseError.UnexpectedToken(values.mkString("One of: ", ", ", ""), start))

  /**
    * Parse one of the expected tokens.
    * 
    * @tparam I the type of a token.
    * @param values the allowed tokens.
    */
  def oneOf[I](values: I*): Parser[I, I] =
    val start = get
    val result = next
    if values.contains(result) then result
    else errorAndAbort(ParseError.UnexpectedToken(values.mkString("One of: ", ", ", ""), start))

  /**
    * Parse one of the expected [[Char]].
    * 
    * @param values the [[String]] representing the allowed characters.
    */
  def oneOf(values: String): Parser[Char, Char] =
    oneOf[Char](values*)

  /**
    * Parse a [[String]] matching the given [[Regex]].
    *
    * @param pattern the regular expression describing the expected pattern.
    */
  def regex(pattern: Regex): Parser[Char, String] =
    pattern.findPrefixOf(remaining.mkString) match
      case Some(value) =>
        advance(value.length)
        value
      case None => errorAndAbort(ParseError.UnexpectedToken(s"Text matching regex $pattern", get))
    
  /**
    * Parse a [[String]] matching the given [[Regex]].
    *
    * @param pattern the regular expression describing the expected pattern.
    */
  def regex(pattern: String): Parser[Char, String] = regex(pattern.r)

  /**
    * Parse a newline (CL, RF or CLRF) character.
    */
  val newline: Parser[Char, Unit] = Parser.firstOf(
    Parser.literal("\r\n"),
    Parser.literal('\r'),
    Parser.literal('\n')
  )

  /**
    * Parse a whitespace character, including newlines.
    * 
    * @see [[inlineWhitespace]] for excluding newlines.
    */
  def whitespace: Parser[Char, Unit] =
    val start = get
    if next.isWhitespace then ()
    else Parser.errorAndAbort(ParseError.UnexpectedToken("Whitespace", start))

  /**
    * Parse a whitespace character. Does not parse newlines.
    * 
    * @see [[whitespace]] for allowing newlines.
    */
  val inlineWhitespace: Parser[Char, Unit] = Parser.andCheck(Parser.whitespace, Parser.not(Parser.newline))

  /**
    * Strip the leading and trailing whitespaces of the given [[Parser]].
    *
    * @tparam A the output type.
    * @param parser the [[Parser]] to pre/post process.
    * @param skipNewlines `true` to skip newline characters, `false` otherwise.
    */
  def spaced[A](parser: Parser[Char, A], skipNewlines: Boolean = true): Parser[Char, A] =
    val skipParser: Parser[Char, Unit] =
      if skipNewlines then Parser.whitespace
      else Parser.inlineWhitespace

    Parser.repeatDiscard0(skipParser)
    val result = parser
    Parser.repeatDiscard0(skipParser)
    result

  /**
    * If the given [[Parser]] succeeds, ignore its output and return the given value instead.
    * 
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parser the [[Parser]] whose output will be discarded.
    * @param value the value to use as output.
    */
  @nowarn("msg=unused")
  def as[I, A](parser: Parser[I, Any], value: A): Parser[I, A] =
    parser
    value

  /**
    * Discard the result of the given [[Parser]].
    * 
    * @tparam I the type of a token.
    * @param parser the [[Parser]] whose output will be discarded.
    */
  inline def unit[I](inline parser: Parser[I, Any]): Parser[I, Unit] = Parser.as(parser, ())

  /**
    * Successively try each [[Parser]], stopping on the first succeeding one.
    * 
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parsers the branches to try.
    */
  def firstOfSeq[I, A](parsers: Seq[Parser[I, A]]): Parser[I, A] = parsers match
    case head +: tail => recover(head)(_ => firstOfSeq(tail))
    case _ => fail(())

  /**
    * Successively try each [[Parser]], stopping on the first succeeding one.
    * 
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parsers the branches to try.
    */
  def firstOf[I, A](parsers: Parser[I, A]*): Parser[I, A] =
    firstOfSeq(parsers)

  /**
    * Run each [[Parser]] sequentially, zipping the outputs.
    * 
    * @tparam I the type of a token.
    * @tparam T the unflattened/unzipped output type.
    * @param parsers the sequence of [[Parser]] to run.
    */
  inline def inOrder[I, T <: Tuple](parsers: T): Parser[I, Zip.All[T]] =
    val parserList = parsers.toList.asInstanceOf[List[Any]]
    val zips = Zip.foldingAll[T]
    parserList
      .zip(zips)
      .foldRight[Any](()):
        case ((value, zip), tail) => zip.zip(value, tail)
      .asInstanceOf[Zip.All[T]]

  /**
    * Check whether the given [[Parser]] succeeded, discarding its output and any state change.
    *
    * @tparam I the type of a token.
    * @param parser the [[Parser]] to try.
    */
  def isSuccessful[I](parser: Parser[I, Any]): Parser[I, Boolean] =
    localState(identity)(recover(Writer(Parser.as(parser, true))._2)(_ => false))

  /**
    * If the given [[Parser]] fails, still fail but with the given [[ParseError]] instead.
    *
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parser the wrapped [[Parser]].
    * @param error the [[ParseError]] to emit in case of failure.
    */
  def orError[I, A](parser: Parser[I, A], error: ParseError): Parser[I, A] =
    recover(parser)(_ => Parser.errorAndAbort(error))

  /**
    * If the given [[Parser]] fails, emit a [[ParseError.UnexpectedToken]] with the given label.
    *
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parser the wrapped [[Parser]].
    * @param expected a label explaining what was expected in case of failure.
    * @see [[orError]] to use another type of [[ParseError]].
    */
  def expect[I, A](parser: Parser[I, A], expected: String): Parser[I, A] =
    val start = get
    Parser.orError(parser, ParseError.UnexpectedToken(expected, start))

  /**
    * Ensure the given [[Parser]] fails.
    *
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parser the [[Parser]] that should fail in order for this one to succeed.
    */
  def not[I, A](parser: Parser[I, A]): Parser[I, Unit] =
    if Parser.isSuccessful(parser) then
      Parser.errorAndAbort(ParseError.UnexpectedToken("Input not validating this parser", get))
    else ()

  /**
    * Check both parsers but only keep the state changes and output of the first one.
    *
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parser the [[Parser]] whose output and state changes will be kept.
    * @param check the [[Parser]] here for additional check. Often using a [[Parser.not]].
    */
  def andCheck[I, A](parser: Parser[I, A], check: Parser[I, Unit]): Parser[I, A] =
    if Parser.isSuccessful(check) then parser
    else Parser.errorAndAbort(ParseError.UnexpectedToken("Something else", get))

  /**
    * In case of failure, recover the given [[Parser]] using the given [[package.RecoverStrategy]].
    *
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parser the wrapped [[Parser]].
    * @param strategy the [[RecoverStrategy]] used to recover from the failure.
    * @return
    */
  def recoverWith[I, A](parser: Parser[I, A], strategy: RecoverStrategy[I, A]): Parser[I, A] =
    recoverKeepLog(parser)(_ => Writer(strategy(parser))._2)

  /**
    * Skip tokens until the given [[Parser]] succeeds.
    *
    * @tparam I the type of a token.
    * @param until the [[Parser]] testing if this one should stop skipping tokens. Does not consume tokens.
    */
  @tailrec
  def skipUntil[I](until: Parser[I, Any]): Parser[I, Unit] =
    if Parser.isSuccessful(until) then ()
    else if Parser.isEOF then errorAndAbort(ParseError.EOF)
    else
      advance(1)
      skipUntil(until)

  /**
    * Repeat a [[Parser]] until another one succeeds. Needs to successfully parse atleast one element to succeed.
    *
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parser the [[Parser]] to repeat.
    * @param until the [[Parser]] testing if the loop should stop. Does not consume tokens.
    * @see [[repeatUntil0]] for allowing empty [[List]].
    */
  def repeatUntil[I, A](parser: Parser[I, A], until: Parser[I, Unit]): Parser[I, List[A]] =

    @tailrec
    def rec(accumulator: List[A]): Parser[I, List[A]] =
      val head = parser
      if Parser.isSuccessful(until) then accumulator :+ head
      else rec(accumulator :+ head)

    rec(Nil)

  /**
    * Repeat a [[Parser]] until another one succeeds. If the other ones immediately succeeds, an empty [[List]] is returned.
    *
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parser the [[Parser]] to repeat.
    * @param until the [[Parser]] testing if the loop should stop. Does not consume tokens.
    * @see [[repeatUntil]] if you expect at least one element to be parsed.
    */
  def repeatUntil0[I, A](parser: Parser[I, A], until: Parser[I, Unit]): Parser[I, List[A]] =
    if Parser.isSuccessful(until) then Nil
    else Parser.repeatUntil(parser, until)

  /**
    * Repeat the same [[Parser]], discarding its result until it fails.
    *
    * @tparam I the type of a token.
    * @param parser the [[Parser]] to repeat.
    */
  @tailrec
  def repeatDiscard0[I](parser: Parser[I, Any]): Parser[I, Unit] =
    if Parser.isEOF || !Parser.isSuccessful(parser) then ()
    else
      Parser.unit(parser)
      Parser.repeatDiscard0(parser)

  /**
    * Repeat the same [[Parser]], separated by another one, until yet another one succeeds.
    * Useful when combined with [[Parser.inOrder]] for patterns such as arrays or function applications.
    *
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parser the [[Parser]] to repeat.
    * @param separator the [[Parser]] separating occurences.
    * @param until the [[Parser]] testing if the loop should stop. Does not consume tokens.
    */
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

  /**
    * Repeat the same [[Parser]], separated by another one, until it fails.
    *
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parser the [[Parser]] to repeat.
    * @param separator the [[Parser]] separating occurences.
    */
  def separatedBy[I, A](parser: Parser[I, A], separator: Parser[I, Unit]): Parser[I, List[A]] =

    @tailrec
    def rec(accumulator: List[A]): Parser[I, List[A]] =
      if Parser.isSuccessful(separator) then
        separator
        rec(accumulator :+ parser)
      else accumulator

    if Parser.isSuccessful(parser) then rec(List(parser))
    else Nil

  /**
    * Repeat the same [[Parser]], separated by another one, until yet another one succeeds.
    * The separator also determines the reduction rule to apply to parsed elements.
    * Useful for parsing patterns such as binary operations. See `formula` example.
    *
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parser the [[Parser]] to repeat.
    * @param separator the [[Parser]] separating occurences.
    * @param until the [[Parser]] testing if the loop should stop. Does not consume tokens.
    */
  def separatedByReduce[I, A](parser: Parser[I, A], separator: Parser[I, (A, A) => A]): Parser[I, A] =

    @tailrec
    def rec(accumulator: A): Parser[I, A] =
      if Parser.isSuccessful(separator) then rec(separator(accumulator, parser))
      else accumulator

    rec(parser)