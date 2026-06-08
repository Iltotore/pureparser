package io.github.iltotore.pureparser.example.json

import io.github.iltotore.pureparser.*
import purelogic.*
import scala.io.Source
import scala.util.Using

enum Json derives CanEqual:
  case JInvalid
  case JNull(span: Span)
  case JBool(value: Boolean, span: Span)
  case JInt(value: Int, span: Span)
  case JString(value: String, span: Span)
  case JArray(elements: List[Json], span: Span)
  case JObject(fields: Map[String, Json], span: Span)

val nullParser: Parser[Char, Json] = Json.JNull(Parser.span(Parser.literal("null")))

val booleanParser: Parser[Char, Json] = Parser.firstOf(
  Json.JBool(true, Parser.span(Parser.literal("true"))),
  Json.JBool(false, Parser.span(Parser.literal("false")))
)

val intParser: Parser[Char, Json] = Json.JInt.apply.tupled(
  Parser.span:
    val start = get
    Parser.regex("[+-]?[0-9]+")
      .toIntOption
      .getOrElse(fail(()))
)

val rawStringParser: Parser[Char, String] = Parser.inOrder(
  Parser.literal('"'),
  Parser.regex("[^\"]+"),
  Parser.literal('"')
)

val stringParser: Parser[Char, Json] = Json.JString.apply.tupled(Parser.span(rawStringParser))

val arrayParser: Parser[Char, Json] = Json.JArray.apply.tupled(
  Parser.span(
    Parser.inOrder(
      Parser.literal('['),
      Parser.separatedByUntil(
        jsonParser,
        Parser.spaced(Parser.literal(',')),
        Parser.literal(']')
      ),
      Parser.literal(']')
    )
  )
)

val fieldParser: Parser[Char, (String, Json)] = Parser.spaced(
  Parser.inOrder(
    rawStringParser,
    Parser.spaced(Parser.literal(':')),
    jsonParser
  )
)

val objectParser: Parser[Char, Json] = Json.JObject.apply.tupled(
  Parser.span(
    Parser.inOrder(
      Parser.literal('{'),
      Parser.separatedByUntil(
        fieldParser,
        Parser.spaced(Parser.literal(',')),
        Parser.literal('}')
      ).toMap,
      Parser.literal('}')
    )
  )
)

val jsonParser: Parser[Char, Json] = Parser.recoverWith(
  Parser.expect(
    Parser.spaced(
      Parser.firstOf(
        objectParser,
        arrayParser,
        nullParser,
        booleanParser,
        intParser,
        stringParser
      )
    ),
    "JSON expression"
  ),
  RecoverStrategy.firstOf(
    RecoverStrategy.nestedDelimiters(
      '[',
      ']',
      Json.JInvalid,
      ('{', '}')
    ),
    RecoverStrategy.nestedDelimiters(
      '{',
      '}',
      Json.JInvalid,
      ('[', ']')
    ),
    RecoverStrategy.skipUntil(Parser.oneOf(",]}"), Json.JInvalid)
  )
)

@main
def run(path: String): Unit =
  Using(Source.fromFile(path))(source =>
    println(pprint(Parser(source.mkString)(jsonParser)))
  ).fold(throw _, identity)
