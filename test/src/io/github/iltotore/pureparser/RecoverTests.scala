package io.github.iltotore.pureparser

import utest.*

object RecoverTests extends TestSuite:

  private enum Value derives CanEqual:
    case VBool(value: Boolean)
    case VArray(values: List[Value])
    case VInvalid

  val tests = Tests:
    val elementParser: Parser[Char, Value] = Parser.expect(
      Parser.firstOf(
        Parser.as(Parser.literal("true"), Value.VBool(true)),
        Parser.as(Parser.literal("false"), Value.VBool(false))
      ),
      "Boolean"
    )

    def arrayParser(element: Parser[Char, Value]): Parser[Char, List[Value]] =
      Parser.inOrder(
        Parser.literal('['),
        Parser.separatedByUntil(
          element,
          Parser.literal(','),
          Parser.literal(']')
        ),
        Parser.literal(']')
      )

    test("skipUntil"):
      val parser: Parser[Char, List[Value]] = arrayParser(
        Parser.recoverWith(elementParser, RecoverStrategy.skipUntil(Parser.oneOf(",]"), Value.VInvalid))
      )

      test("untilSeparator") - assertSuccess(parser, "[true,invalid,false]")(List(Value.VBool(true), Value.VInvalid, Value.VBool(false)))
      test("untilEnd") - assertSuccess(parser, "[true,false,invalid]")(List(Value.VBool(true), Value.VBool(false), Value.VInvalid))
      test("both") - assertSuccess(parser, "[true,invalid,invalid]")(List(Value.VBool(true), Value.VInvalid, Value.VInvalid))

    test("skipThenRetryUntil"):
      val parser: Parser[Char, List[Value]] = arrayParser(
        Parser.recoverWith(elementParser, RecoverStrategy.skipThenRetryUntil(Parser.oneOf(",]")))
      )

      test("invalidPrefixBeforeSeparator") - assertSuccess(parser, "[true,invalidtrue,false]")(List(Value.VBool(true), Value.VBool(true), Value.VBool(false)))
      test("invalidPrefixBeforeEnd") - assertSuccess(parser, "[true,true,invalidfalse]")(List(Value.VBool(true), Value.VBool(true), Value.VBool(false)))
      test("invalidSuffix") - assertErrors(parser, "[true,trueinvalid,false]"):
        case Seq(ParseError.UnexpectedToken(_, 10)) =>
      test("entirelyInvalid") - assertErrors(parser, "[true,invalid,false]"):
        case Seq(ParseError.UnexpectedToken(_, 6)) =>

    test("nestedDelimiters"):
      lazy val parser: Parser[Char, List[Value]] = arrayParser(
        Parser.recoverWith(
          Parser.expect(
            Parser.firstOf(
              elementParser,
              Value.VArray(parser)
            ),
            "Value"
          ),
          RecoverStrategy.nestedDelimiters('[', ']', Value.VInvalid)
        )
      )

      test("invalidNestedElement") - assertSuccess(parser, "[[true,invalid,false],[false,true]]")(List(
        Value.VInvalid,
        Value.VArray(List(Value.VBool(false), Value.VBool(true)))
      ))

      test("invalidSeparator") - assertSuccess(parser, "[[true,true false],[false,true]]")(List(
        Value.VInvalid,
        Value.VArray(List(Value.VBool(false), Value.VBool(true)))
      ))

    test("mixed"):
      lazy val parser: Parser[Char, List[Value]] = arrayParser(
        Parser.recoverWith(
          Parser.expect(
            Parser.firstOf(
              elementParser,
              Value.VArray(parser)
            ),
            "Value"
          ),
          RecoverStrategy.firstOf(
            RecoverStrategy.nestedDelimiters('[', ']', Value.VInvalid),
            RecoverStrategy.skipThenRetryUntil(Parser.oneOf(",]")),
            RecoverStrategy.skipUntil(Parser.oneOf(",]"), Value.VInvalid)
          )
        )
      )

      test("invalid") - assertSuccess(parser, "[true,invalid,false]")(List(Value.VBool(true), Value.VInvalid, Value.VBool(false)))
      test("invalidPrefix") - assertSuccess(parser, "[true,invalidtrue,false]")(List(Value.VBool(true), Value.VBool(true), Value.VBool(false)))
      test("mixOfAll") - assertSuccess(parser, "[[true;false],[invalid,invalidtrue,false]]")(List(
        Value.VInvalid,
        Value.VArray(List(Value.VInvalid, Value.VBool(true), Value.VBool(false)))
      ))