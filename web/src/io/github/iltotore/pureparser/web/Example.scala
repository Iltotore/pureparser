package io.github.iltotore.pureparser.web

import io.github.iltotore.pureparser.Parser
import io.github.iltotore.pureparser.example.*
import scala.quoted.*
import io.github.iltotore.pureparser.example.formula.Expr
import io.github.iltotore.pureparser.example.indent.Statement
import io.github.iltotore.pureparser.example.json.Json

case class Example(name: String, parserFunction: () => Parser[Char, Tree], samples: Map[String, String]):

  //Unfortunately, Scala 3 does not support context functions as case class parameters
  val parser: Parser[Char, Tree] = parserFunction()

object Example:

  given ToTree[Expr] = ToTree.derived
  given ToTree[Statement] = ToTree.derived
  given ToTree[Json] = ToTree.derived
  
  val defaultSample: String = loadCompileTime("sample.json")

  val defaultSelection: Example = Example(
    name = "JSON",
    parserFunction = () => ToTree("Root", json.jsonParser),
    samples = Map(
      "Valid" -> defaultSample,
      "Partially invalid" -> loadCompileTime("sample_broken.json"),
    )
  )

  val defaults: List[Example] = List(
    defaultSelection,
    Example(
      name = "Math Formula",
      parserFunction = () => ToTree("Root", formula.exprParser),
      samples = Map(
        "Valid" -> loadCompileTime("sample.formula"),
        "Partially invalid" -> loadCompileTime("sample_broken.formula")
      )
    ),
    Example(
      name = "Indentation",
      parserFunction = () => ToTree("Root", indent.statementsParser),
      samples = Map(
        "Valid" -> loadCompileTime("sample.indentation"),
        "Partially invalid" -> loadCompileTime("sample_broken.indentation")
      )
    ),
  )