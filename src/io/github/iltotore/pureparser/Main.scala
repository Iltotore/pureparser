package io.github.iltotore.pureparser

object Main:

  //[1, 1, 2, [...]]
  //Array(Literal(1), Literal(1), Literal(2), Array(...))

  enum Expr:
    case Literal(value: Int)
    case Array(elements: List[Expr])
    case Invalid

  val literalParser: Parser[Char, Expr] = Expr.Literal(
    Parser.expect(
      Parser
        .regex("[0-9]+")
        .toIntOption
        .getOrElse(Parser.abort),
        "Literal int"
    )
  )

  val arrayParser: Parser[Char, Expr] = Expr.Array(
    Parser.inOrder(
      Parser.literal('['),
      Parser.separatedByUntil(exprParser, Parser.literal(','), Parser.literal(']')),
      Parser.literal(']')
    )
  )

  val exprParser: Parser[Char, Expr] = Parser.recoverWith(
    Parser.expect(
      Parser.firstOf(
        literalParser,
        arrayParser
      ),
      "Expression"
    ),
    RecoverStrategy.skipUntil(Parser.oneOf(",]"), Expr.Invalid)
  )

  @main
  def run: Unit =
    println(Parser("1")(exprParser))
    println(Parser("[1,2,3]")(exprParser))
    println(Parser("[1,[2,3]]")(exprParser))
    println(Parser("[1,[2,3,a]]")(exprParser))