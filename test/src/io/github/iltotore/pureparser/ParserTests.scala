package io.github.iltotore.pureparser

import utest.*

object ParserTests extends TestSuite:

  val tests = Tests:
    test("peek"):
      test("single") - assertSuccess(Parser.peek, "a")('a', 0)
      test("multiple"):
        val parser: Parser[Char, Char] =
          Parser.unit(Parser.peek)
          Parser.peek

        assertSuccess(parser, "ab")('a', 0)

    test("next"):
      test("single") - assertSuccess(Parser.next, "a")('a')
      test("multiple"):
        val parser: Parser[Char, Char] =
          Parser.unit(Parser.next)
          Parser.next

        assertSuccess(parser, "ab")('b')

      test("eof") - assertErrors(Parser.next, ""):
        case Seq(ParseError.EOF) =>

    test("eof"):
      test("empty") - assertSuccess(Parser.eof, "")(())
      test("hasRemaining") - assertErrors(Parser.eof, "a"):
        case Seq(ParseError.UnexpectedToken(_, 0)) =>

    test("span"):
      test("empty") - assertSuccess(Parser.span(Parser.eof), "")(Span(0, 0))
      test("nonEmpty") - assertSuccess(Parser.span(Parser.next[Char]), "a")(('a', Span(0, 1)))

    test("literal"):
      test("token") - assertSuccess(Parser.literal('a'), "a")(())
      test("string") - assertSuccess(Parser.literal("abc"), "abc")(())
      test("unexpectedToken") - assertErrors(Parser.literal('a'), "b"):
        case Seq(ParseError.UnexpectedToken(_, 0)) =>
      test("unexpectedString") - assertErrors(Parser.literal("abc"), "abd"):
        case Seq(ParseError.UnexpectedToken(_, 0)) =>
      test("eof") - assertErrors(Parser.literal('a'), ""):
        case Seq(ParseError.EOF) =>

    test("oneOf"):
      val parserSeq: Parser[Char, Char] = Parser.oneOf('a', 'b')
      val parserString: Parser[Char, Char] = Parser.oneOf("ab")

      test("seq"):
        assertSuccess(parserSeq, "a")('a')
        assertSuccess(parserSeq, "b")('b')

      test("string"):
        assertSuccess(parserString, "a")('a')
        assertSuccess(parserString, "b")('b')

      test("none") - assertErrors(parserSeq, "c"):
        case Seq(ParseError.UnexpectedToken(_, 0)) =>

    test("regex"):
      val parser: Parser[Char, String] = Parser.regex("[0-9]+")
      test("full") - assertSuccess(parser, "1234")("1234")
      test("part") - assertSuccess(parser, "1234abcd")("1234", 4)
      test("unexpected") - assertErrors(parser, "abcd1234"):
        case Seq(ParseError.UnexpectedToken(_, 0)) =>

    test("spaced"):
      val parser: Parser[Char, Unit] = Parser.spaced(Parser.literal('a'))

      test("noSpace") - assertSuccess(parser, "a")(())
      test("leadingSpaces") - assertSuccess(parser, "\t\n a")(())
      test("trailingSpaces") - assertSuccess(parser, "a \t\n")(())
      test("both") - assertSuccess(parser, "\t\n a \t\n")(())

    test("as"):
      val parser: Parser[Char, Boolean] = Parser.as(Parser.literal("true"), true)

      test("success") - assertSuccess(parser, "true")(true)
      test("unexpected") - assertErrors(parser, "false"):
        case Seq(ParseError.UnexpectedToken(_, 0)) =>

    test("firstOf"):
      val parser: Parser[Char, Boolean] = Parser.firstOf(
        Parser.as(Parser.literal("true"), true),
        Parser.as(Parser.literal("false"), false)
      )

      test("true") - assertSuccess(parser, "true")(true)
      test("false") - assertSuccess(parser, "false")(false)
      test("unexpected") - assertErrors(parser, "well yes but actually no"):
        case Seq() =>

    test("inOrder"):
      val parser: Parser[Char, Boolean] = Parser.inOrder(
        Parser.literal('('),
        Parser.as(Parser.literal("true"), true),
        Parser.literal(')')
      )

      test("success") - assertSuccess(parser, "(true)")(true)
      test("missingParenthesisOpen") - assertErrors(parser, "true)"):
        case Seq(ParseError.UnexpectedToken(_, 0)) =>
      test("missingParenthesisClose") - assertErrors(parser, "(true"):
        case Seq(ParseError.EOF) =>
      test("missingTrue") - assertErrors(parser, "()"):
        case Seq(ParseError.UnexpectedToken(_, 1)) =>

    test("isSuccessful"):
      val parser: Parser[Char, Boolean] = Parser.isSuccessful(Parser.literal('a'))

      test("true") - assertSuccess(parser, "a")(true, 0)
      test("false"):
        test("unexpected") - assertSuccess(parser, "b")(false, 0)
        test("eof") - assertSuccess(parser, "")(false, 0)

    test("orError"):
      val parser: Parser[Char, Unit] = Parser.orError(Parser.literal('a'), ParseError.UnexpectedToken("The letter a", 0))

      test("success") - assertSuccess(parser, "a")(())
      test("failure"):
        test("unexpected") - assertErrors(parser, "b"):
          case Seq(ParseError.UnexpectedToken("The letter a", 0)) =>
        test("eof") - assertErrors(parser, ""):
          case Seq(ParseError.UnexpectedToken("The letter a", 0)) =>

    test("not"):
      val parser: Parser[Char, Unit] = Parser.not(Parser.literal('a'))

      test("success"):
        test("notA") - assertSuccess(parser, "b")((), 0)
        test("eof") - assertSuccess(parser, "")((), 0)
      test("a") - assertErrors(parser, "a"):
        case Seq(ParseError.UnexpectedToken(_, 0)) =>

    test("andCheck"):
      val parser: Parser[Char, Char] = Parser.andCheck(
        Parser.next,
        Parser.literal("abc")
      )

      test("success") - assertSuccess(parser, "abc")('a', 1)
      test("failure") - assertErrors(parser, "a"):
        case Seq(ParseError.UnexpectedToken(_, 0)) =>

    test("skipUntil"):
      val parser: Parser[Char, Unit] = Parser.skipUntil(Parser.literal('b'))

      test("success") - assertSuccess(parser, "aaab")((), 3)
      test("onlyUntil") - assertSuccess(parser, "b")((), 0)
      test("eof") - assertErrors(parser, "aaa"):
        case Seq(ParseError.EOF) =>
      test("untilEOF") - assertSuccess(Parser.skipUntil(Parser.eof), "aaa")(())

    test("repeatUntil"):
      val parser: Parser[Char, List[Char]] = Parser.repeatUntil(Parser.oneOf("ab"), Parser.literal("END"))

      test("success") - assertSuccess(parser, "abaaEND")(List('a', 'b', 'a', 'a'), 4)
      test("onlyUntil") - assertErrors(parser, "END"):
        case Seq(ParseError.UnexpectedToken(_, 0)) =>
      test("unexpectedElement") - assertErrors(parser, "abacEND"):
        case Seq(ParseError.UnexpectedToken(_, 3)) =>
      test("noUntil") - assertErrors(parser, "abaa"):
        case Seq(ParseError.EOF) =>

    test("repeatUntil0"):
      val parser: Parser[Char, List[Char]] = Parser.repeatUntil0(Parser.oneOf("ab"), Parser.literal("END"))

      test("success") - assertSuccess(parser, "abaaEND")(List('a', 'b', 'a', 'a'), 4)
      test("onlyUntil") - assertSuccess(parser, "END")(Nil, 0)
      test("unexpectedElement") - assertErrors(parser, "abacEND"):
        case Seq(ParseError.UnexpectedToken(_, 3)) =>
      test("noUntil") - assertErrors(parser, "abaa"):
        case Seq(ParseError.EOF) =>

    test("repeatDiscard0"):
      val parser: Parser[Char, Unit] = Parser.repeatDiscard0(Parser.literal("ab"))

      test("success") - assertSuccess(parser, "ababab")(())
      test("untilUnexpected"):
        assertSuccess(parser, "ababc")((), 4)
        assertSuccess(parser, "ababa")((), 4)
      test("onlyUnexpected") - assertSuccess(parser, "c")((), 0)
      test("eof") - assertSuccess(parser, "")(())

    test("separatedByUntil"):
      val parser: Parser[Char, List[Char]] = Parser.separatedByUntil(
        Parser.oneOf("ab"),
        Parser.literal(','),
        Parser.literal("END")
      )

      test("single") - assertSuccess(parser, "aEND")(List('a'), 1)
      test("multiple") - assertSuccess(parser, "a,bEND")(List('a', 'b'), 3)
      test("onlyUntil") - assertSuccess(parser, "END")(Nil, 0)
      test("unexpectedElement") - assertErrors(parser, "a,cEND"):
        case Seq(ParseError.UnexpectedToken(_, 2)) =>
      test("noUntil") - assertErrors(parser, "a,b"):
        case Seq(ParseError.EOF) =>