package io.github.iltotore.pureparser

import purelogic.*

/**
  * A recovery strategy, basically a recovery [[Parser]] based on the original one.
  * 
  * @tparam I the type of a token.
  * @tparam A the output type.
  */
trait RecoverStrategy[I, A]:

  /**
    * Recover the given [[Parser]].
    *
    * @param parser the failing [[Parser]] to recover.
    * @return a [[Parser]] recovering the given one.
    */
  def apply(parser: Parser[I, A]): Parser[I, A]

object RecoverStrategy:

  /**
    * Successively try each [[RecoverStrategy]], stopping on the first succeeding one.
    * 
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param strategies the branches to try.
    */
  def firstOfSeq[I, A](strategies: Seq[RecoverStrategy[I, A]]): RecoverStrategy[I, A] = new RecoverStrategy:
    override def apply(parser: Parser[I, A]): Parser[I, A] = strategies match
      case head +: tail => recover(head(parser))(_ => firstOfSeq(tail)(parser))
      case _ => Parser.abort

  /**
    * Successively try each [[RecoverStrategy]], stopping on the first succeeding one.
    * 
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parsers the branches to try.
    */
  def firstOf[I, A](strategies: (RecoverStrategy[I, A])*): RecoverStrategy[I, A] =
    firstOfSeq(strategies)

  /**
    * Try the given [[Parser]] instead.
    *
    * @param parser the [[Parser]] to try.
    */
  def viaParser[I, A](parser: Parser[I, A]): RecoverStrategy[I, A] = new RecoverStrategy:
    override def apply(ignored: Parser[I, A]): Parser[I, A] = parser

  /**
    * Skip tokens until the given [[Parser]] succeeds
    *
    * @tparam I the type of a token.
    * @param until the [[Parser]] testing if this one should stop skipping tokens. Does not consume tokens.
    * @param fallback the fallback value, usually representing an "invalid" node.
    * @return the fallback value.
    */
  def skipUntil[I, A](until: Parser[I, Any], fallback: A): RecoverStrategy[I, A] = new RecoverStrategy:
    override def apply(parser: Parser[I, A]): Parser[I, A] = Parser.as(Parser.skipUntil(until), fallback)

  /**
    * Skip a token then retry the failing [[Parser]] until it succeeds or until the given [[Parser]] succeeds.
    *
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param until the [[Parser]] testing if the strategy should stop retrying the failing parser and fail instead. Does not consume tokens.
    */
  def skipThenRetryUntil[I, A](until: Parser[I, Any]): RecoverStrategy[I, A] = new RecoverStrategy:
    override def apply(parser: Parser[I, A]): Parser[I, A] =
      def rec: Parser[I, A] =
        if Parser.isEOF || Parser.isSuccessful(until) then fail(())
        else
          Parser.advance(1)
          Parser.firstOf(parser, rec)

      rec

  /**
    * Skip smartly by searching for a start and end delimiter, respecting nesting. For example, parsing `( <invalid expression> ( ... ) )` 
    * would skip the entire expression instead of stopping at the first closing parenthesis like [[skipUntil]] would do, avoiding
    * cascading errors.
    *
    * @param start the start delimiter.
    * @param end the end delimiter.
    * @param fallback the fallback value, usually representing an "invalid" node.
    * @param otherDelimiters to better detect delimiter mismatch. Typically, defining `{}` delimiters when recovering from parenthesized
    * expression parsing can help recovering from patterns such as `( ... }` or `( ... { ... )`.
    * @return the fallback value.
    */
  def nestedDelimiters[I, A](start: I, end: I, fallback: A, otherDelimiters: (I, I)*)(using CanEqual[I, I]): RecoverStrategy[I, A] = new RecoverStrategy:
    override def apply(parser: Parser[I, A]): Parser[I, A] =
      def outerBlock: Parser[I, Any] = Parser.inOrder(Parser.literal(start), rec, Parser.literal(end))

      //The compiler needs a bit of help to avoid inferring the wrong type/eagerly evaluate the context functions
      def nestedBlocks: Parser[I, Any] = Parser.firstOfSeq(
        outerBlock +: otherDelimiters.map[Parser[I, Any]]((s, e) => Parser.inOrder(Parser.literal(s), rec, Parser.literal(end)))
      )

      def allDelimiterTokens = start +: end +: otherDelimiters.flatMap(Seq(_, _))
      def allDelimiters: Parser[I, Unit] = Parser.unit(Parser.oneOf(allDelimiterTokens*))

      def rec: Parser[I, Any] = Parser.repeatDiscard0(
        Parser.firstOf(
          nestedBlocks,
          Parser.andCheck(Parser.next, Parser.not(allDelimiters))
        )
      )

      Parser.inOrder(Parser.literal(start), Parser.unit(rec), Parser.literal(end))
      fallback