---
title: Overview
---

# Overview

PureParser is a parser combinators library just like [Scala Parser Combinators](https://github.com/scala/scala-parser-combinators) or
[FastParse](https://com-lihaoyi.github.io/fastparse/) with two important additional features: multiple errors support and AST recovery.

## Why parser combinators?

There are roughly three main ways to write a parser:
- Handwritting
- Parser generators
- Parser combinators

The latter approach shines by its composability, integration with existing code and ease of refactoring.
This is because with parser combinators, parsers are normal code that you can compose using functions (combinators) to create more
complex parsers. Since parsers are normal code, you can easily integrate them to your codebase and refactor them however you want.

Given this representation:

```scala
enum Token:
  case For
  case While
  case Do
  // ...
```

the following code:

```scala
val forParser: Parser[Char, Expr] = Parser.as(Parser.literal("for"), Token.For)
val whileParser: Parser[Char, Expr] = Parser.as(Parser.literal("while"), Token.While)
val doParser: Parser[Char, Expr] = Parser.as(Parser.literal("do"), Token.Do)
// ...
```

can be refactored into:

```scala
val keywords: Map[String, Token] = Map(
  "for"   -> Token.For,
  "while" -> Token.While,
  "do"    -> Token.For
  // ...
)

val keywordParser: Parser[Char, Expr] = keywords.getOrElse(Parser.regex("[a-zA-Z]+"), Parser.abort)
```

## The case for PureParser

Almost all parser combinators library focus on the happy path and mostly ignore the failing cases which are in many use cases
(i.e programming languages) business errors: they are expected to happen under some conditions and need to be gracefully handled.

PureParser handle those cases by allowing AST recovery and emission of multiple error messages if multiple parts of the input are invalid.

Using the [indentation example](https://github.com/Iltotore/pureparser/blob/main/examples/indentation.scala):

```scala
// Other parsers...

// Where the recovery is defined
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
```

this code:

```
expr
typoexpr
loop:
  expr
  loop:
    yo
    expr
    loop:

expr
```

Is understood as:

```
expr
expr
loop:
  expr
  loop:
    <invalid expression>
    expr
    loop:

expr
```

with the following errors:

```scala
UnexpectedToken(expected = "Statement", at = 5)
UnexpectedToken(expected = "Statement", at = 39)
UnexpectedToken(expected = "Greater indentation than 4. Currently: 0", at = 62)
```

Those features make PureParser suited for:

- Compilers
- Code tooling such as linters, formatters...

Note that PureParser was not specifically designed with performance in mind since in the cases previously mentionned it is not the bottleneck.

## Getting started

Check the [Getting Started](getting-started.md) to import and start using PureParser.