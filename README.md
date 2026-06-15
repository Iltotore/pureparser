# PureParser

A parser combinator library built on [PureLogic](https://ghostdogpr.github.io/purelogic/) in Scala 3, featuring :

- Usual benefits of parser combinators (ease of use, reusability...)
- Accumulation of all relevant errors unlike short-circuiting behaviors like `Either`
- AST recovery so that even partially-invalid code still produces a partial output

These two last features make PureParser useful for implementing mature parsers for programming languages or LSP implementations.

## Example

```scala
enum Expr:
  case Literal(value: Int)
  case Array(elements: List[Expr])
  case Invalid

val literalParser: Parser[Char, Expr] = Expr.Literal(
  Parser
    .regex("[+-]?[0-9]+")
    .toIntOption
    .getOrElse(Parser.abort)
)

val arrayParser: Parser[Char, Expr] = Expr.Array(
  Parser.inOrder(
    Parser.literal('['),
    Parser.separatedByUntil(exprParser, Parser.literal(','), Parser.literal(']')),
    Parser.literal(']')
  )
)

val exprParser: Parser[Char, Expr] = Parser.recoverWith(
  Parser.expect(Parser.firstOf(literalParser, arrayParser), "Valid expression"),
  RecoverStrategy.skipUntil(Parser.oneOf(",]"), Expr.Invalid)
)

// Literal(1)
println(Parser("1")(exprParser))

// Array(List(Literal(1), Literal(2)))
println(Parser("[1,2]")(exprParser))

// Array(List(Literal(1), Literal(2), Invalid, Literal(4), Invalid, Literal(5)))
// + two errors "Unexpected token, expected: Valid expression" at position 5 and 13
println(Parser("[1,2,hello,4,there,5]")(exprParser))
```

Play with more examples on the [example playground](https://iltotore.github.io/pureparser/examples/) ([sources](examples)).

## Import in your project

SBT:

```scala
libraryDependencies += "io.github.iltotore" %% "pureparser" % "version"
```

Mill:

```scala
mvn"io.github.iltotore::pureparser:version"
```

Note: replace `version` with the version of PureParser you want to use.

PureParser support Scala JVM and JS.

## Useful links

- [Scaladoc](https://iltotore.github.io/pureparser/)