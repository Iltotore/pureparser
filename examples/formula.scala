package io.github.iltotore.pureparser.example.formula

import io.github.iltotore.pureparser.*
import purelogic.*
import scala.io.Source
import scala.util.Using

enum Expr:
  case Invalid(span: Span)
  case Literal(value: Double, span: Span)
  case Add(left: Expr, right: Expr, span: Span)
  case Sub(left: Expr, right: Expr, span: Span)
  case Mul(left: Expr, right: Expr, span: Span)
  case Div(left: Expr, right: Expr, span: Span)

  def span: Span

/**
  * This literal number parser could be a regex but is enhanced here to showcase how one can provide
  * more precise errors than "expected valid number/expression".
  * 
  * Here, this parser can tell:
  * - if the decimal part after `.` is missing -> recover the decimal part as 0
  * - if the user entered `,` for the decimal part instead of `.` -> recover the number as if `.` was used
  * - if the number is invalid for any other reason
  */
val literalParser: Parser[Char, Expr] =
  val (intPart, decimalPart, span) = Parser.span(
    Parser.inOrder(
      Parser.regex("[0-9]+"),
      Parser.recoverWith(
        Parser.firstOf(
          Parser.inOrder(
            Parser.recoverWith(
              Parser.literal('.'),
              RecoverStrategy.viaParser(Parser.literal(','))
            ),
            Parser.commit(Parser.expect(Parser.regex("[0-9]+"), "Decimal part of number"))
          ),
          "0"
        ),
        RecoverStrategy.skipUntil(Parser.oneOf(binaryAddOps.keySet ++ binaryMulOps.keySet + ')'), "0")
      )
    )
  )
  
  Expr.Literal(
    s"$intPart.$decimalPart"
      .toDoubleOption
      .getOrElse(Parser.errorAndAbort(ParseError("Valid number", span.start))),
    span
  )


val parenthesizedParser: Parser[Char, Expr] = Parser.inOrder(
  Parser.literal('('),
  exprParser,
  Parser.literal(')')
)

val termParser: Parser[Char, Expr] = Parser.recoverWith(
  Parser.expect(Parser.firstOf(literalParser, parenthesizedParser), "Literal or parenthesized expression"),
  RecoverStrategy.firstOf(
    RecoverStrategy.nestedDelimiters('(', ')', Expr.Invalid(Span(0, 0))),
    RecoverStrategy.skipUntil(Parser.firstOf(Parser.oneOf("+-*/)"), Parser.eof), Expr.Invalid(Span(0, 0)))
  )
)

val binaryAddOps: Map[Char, (Expr, Expr, Span) => Expr] = Map(
  '+' -> Expr.Add.apply,
  '-' -> Expr.Sub.apply
)

val binaryMulOps: Map[Char, (Expr, Expr, Span) => Expr] = Map(
  '*' -> Expr.Mul.apply,
  '/' -> Expr.Div.apply
)

val allOperators = binaryAddOps.keySet ++ binaryMulOps.keySet

def binaryOpsParser(operandParser: Parser[Char, Expr], operators: Map[Char, (Expr, Expr, Span) => Expr]): Parser[Char, Expr] =
  Parser.separatedByReduce(
    operandParser,
    locally:
      val operatorKeys = operators.keySet
      val operator = Parser.recoverWith(
        operators(Parser.spaced(Parser.expect(Parser.oneOf(operatorKeys), "Valid binary operator"))),
        RecoverStrategy.skipThenRetryUntil(Parser.oneOf(allOperators -- operatorKeys + ')'))
      )
      (left, right) => operator(left, right, left.span.merge(right.span))
  )

val binaryMulParser: Parser[Char, Expr] = binaryOpsParser(termParser, binaryMulOps)
val binaryAddParser: Parser[Char, Expr] = binaryOpsParser(binaryMulParser, binaryAddOps)

val exprParser: Parser[Char, Expr] = Parser.recoverWith(
  Parser.expect(Parser.spaced(binaryAddParser), "Valid expression"),
  RecoverStrategy.firstOf(
    RecoverStrategy.nestedDelimiters('(', ')', Expr.Invalid(Span(0, 0))),
    RecoverStrategy.skipUntil(Parser.firstOf(Parser.literal(')'), Parser.eof), Expr.Invalid(Span(0, 0)))
  )
)

@main
def run(path: String): Unit =
  Using(Source.fromFile(path))(source =>
    println(pprint(Parser(source.mkString)(exprParser)))
  ).fold(throw _, identity)
