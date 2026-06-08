package io.github.iltotore.pureparser.example.indent

import io.github.iltotore.pureparser.*
import io.github.iltotore.pureparser.util.ByName
import purelogic.*
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

enum Statement:
  case Invalid
  case Expr(span: Span)
  case Loop(statements: List[Statement], span: Span)

val exprParser: Parser[Char, Statement] = Statement.Expr(Parser.span(Parser.literal("expr")))

def loopParser(indentation: Int): Parser[Char, Statement] = Statement.Loop.apply.tupled(
  Parser.span(
    Parser.inOrder(
      Parser.literal("loop:"),
      Parser.newline,
      blockParser(indentation)
    )
  )
)

val skipInlineWhitespace: Parser[Char, Unit] = Parser.repeatDiscard0(Parser.inlineWhitespace)

val indentationParser: Parser[Char, Int] = Parser.inOrder(
  Parser.repeatDiscard0(Parser.inOrder(skipInlineWhitespace, Parser.newline)),
  Parser.span(skipInlineWhitespace).size,
  Parser.not(Parser.eof)
)

def blockParser(indentation: Int): Parser[Char, List[Statement]] =
  val startIndentation = indentationParser
  if startIndentation <= indentation then
    write(ParseError.UnexpectedToken(s"Greater indentation than $indentation. Currently: $startIndentation", get))
    Nil
  else
    val sameIndentationParser: Parser[Char, Unit] =
      val lineIdentation = indentationParser
      if lineIdentation != startIndentation then
        Parser.errorAndAbort(ParseError.UnexpectedToken(s"Same indentation than $startIndentation. Currently: $lineIdentation", get))
      
    Parser.separatedBy(statementParser(startIndentation), sameIndentationParser)

val newlineOrEOF: Parser[Char, Unit] = Parser.firstOf(Parser.newline, Parser.eof)

def statementParser(indentation: Int): Parser[Char, Statement] = Parser.recoverWith(
  Parser.expect(
    Parser.firstOf(
      Parser.inOrder(exprParser, newlineOrEOF),
      loopParser(indentation)
    ),
    "Statement"
  ),
  RecoverStrategy.firstOf(
    RecoverStrategy.skipThenRetryUntil(newlineOrEOF),
    RecoverStrategy.skipUntil(newlineOrEOF, Statement.Invalid)
  )
)

val statementsParser: Parser[Char, List[Statement]] = blockParser(-1)

@main
def run(path: String): Unit =
  Using(Source.fromFile(path))(source =>
    println(pprint(Parser(source.mkString)(statementsParser)))
  ).fold(throw _, identity)