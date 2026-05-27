package io.github.iltotore.pureparser

import io.github.iltotore.pureparser.util.ByName
import purelogic.*

trait RecoverStrategy[I, A]:
  def apply(parser: Parser[I, A]): Parser[I, A]

object RecoverStrategy:

  def firstOfSeq[I, A](strategies: Seq[ByName ?=> RecoverStrategy[I, A]]): RecoverStrategy[I, A] = new RecoverStrategy:
    override def apply(parser: Parser[I, A]): Parser[I, A] = strategies match
      case head +: tail => recover(head(using ByName)(parser))(_ => firstOfSeq(tail)(parser))
      case _ => fail(())

  def firstOf[I, A](strategies: (ByName ?=> RecoverStrategy[I, A])*): RecoverStrategy[I, A] =
    firstOfSeq(strategies)

  def skipUntil[I, A](until: Parser[I, Any], fallback: A): RecoverStrategy[I, A] = new RecoverStrategy:
    override def apply(parser: Parser[I, A]): Parser[I, A] = Parser.as(Parser.skipUntil(until), fallback)

  def skipThenRetryUntil[I, A](until: Parser[I, Any]): RecoverStrategy[I, A] = new RecoverStrategy:
    override def apply(parser: Parser[I, A]): Parser[I, A] =
      def rec: Parser[I, A] =
        if Parser.isEOF || Parser.isSuccessful(until) then fail(())
        else
          Parser.advance(1)
          Parser.firstOf(parser, rec)

      rec

  def nestedDelimiters[I, A](start: I, end: I, fallback: A, otherDelimiters: (I, I)*)(using CanEqual[I, I]): RecoverStrategy[I, A] = new RecoverStrategy:
    override def apply(parser: Parser[I, A]): Parser[I, A] =
      def outerBlock: Parser[I, Any] = Parser.between(Parser.literal(start), rec, Parser.literal(end))

      //The compiler needed a bit of help to avoid inferring the wrong type/eagerly evaluate the context functions
      def nestedBlocks: Parser[I, Any] = Parser.firstOfSeq(
        outerBlock +: otherDelimiters.map[ByName ?=> Parser[I, Any]]((s, e) => Parser.between(Parser.literal(s), rec, Parser.literal(end)))
      )

      def allDelimiterTokens = start +: end +: otherDelimiters.flatMap(Seq(_, _))
      def allDelimiters: Parser[I, Unit] = Parser.unit(Parser.oneOf(allDelimiterTokens*))

      def rec: Parser[I, Any] = Parser.repeatDiscard0(
        Parser.firstOf(
          nestedBlocks,
          Parser.andCheck(Parser.next, Parser.not(allDelimiters))
        )
      )

      Parser.between(Parser.literal(start), rec, Parser.literal(end))
      fallback