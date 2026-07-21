package io.github.iltotore.pureparser

import utest.*
import io.github.iltotore.pureparser.ParseError.Pattern

object ParserTests extends TestSuite:

  private enum Expr derives CanEqual:
    case VarCall(name: String)
    case Assign(name: String, expr: String)

  private enum Token:
    case Literal(value: Int, span: Span)
    case ParenOpen(span: Span)
    case ParenClosed(span: Span)

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
        case Seq(ParseError(ParseError.Pattern.SomethingElse, 0)) =>

    test("eof"):
      test("empty") - assertSuccess(Parser.eof, "")(())
      test("hasRemaining") - assertErrors(Parser.eof, "a"):
        case Seq(ParseError(ParseError.Pattern.EOF, 0)) =>

    test("span"):
      test("empty") - assertSuccess(Parser.span(Parser.eof), "")(Span(0, 0))
      test("nonEmpty") - assertSuccess(Parser.span(Parser.next[Char]), "a")(('a', Span(0, 1)))

    test("literal"):
      test("token") - assertSuccess(Parser.literal('a'), "a")(())
      test("string") - assertSuccess(Parser.literal("abc"), "abc")(())
      test("unexpectedToken") - assertErrors(Parser.literal('a'), "b"):
        case Seq(ParseError(ParseError.Pattern.Token('a'), 0)) =>
      test("unexpectedString") - assertErrors(Parser.literal("abc"), "abd"):
        case Seq(ParseError(ParseError.Pattern.Label("abc"), 0)) =>
      test("eof") - assertErrors(Parser.literal('a'), ""):
        case Seq(ParseError(ParseError.Pattern.Token('a'), 0)) =>

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
        case Seq(ParseError(ParseError.Pattern.Label(_), 0)) =>

    test("regex"):
      val parser: Parser[Char, String] = Parser.regex("[0-9]+")
      test("full") - assertSuccess(parser, "1234")("1234")
      test("part") - assertSuccess(parser, "1234abcd")("1234", 4)
      test("unexpected") - assertErrors(parser, "abcd1234"):
        case Seq(ParseError(ParseError.Pattern.Label(_), 0)) =>

    test("matching"):
      val parser: Parser[Char, Int] = Parser.matching[Char, Int]:
        case 'a' => 1
        case 'b' => 2

      test("success"):
        test - assertSuccess(parser, "a")(1)
        test - assertSuccess(parser, "b")(2)

      test("unexpected") - assertErrors(parser, "c"):
        case Seq(ParseError(Pattern.SomethingElse, 0)) =>

    test("ofType"):
      val parser: Parser[Token, Int] = Parser.inOrder(
        Parser.ofType[Token, Token.ParenOpen],
        Parser.matching[Token, Int]:
          case Token.Literal(value, _) => value,
        Parser.ofType[Token, Token.ParenClosed]
      )

      test("success") - assertSuccessTokens(
        parser,
        Token.ParenOpen(Span(0, 0)),
        Token.Literal(55, Span(1, 3)),
        Token.ParenClosed(Span(3, 4))
      )(55)

      test("unexpected") - assertErrorsTokens(parser, Token.Literal(55, Span(0, 2))):
        case Seq(ParseError(Pattern.SomethingElse, 0)) =>

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
        case Seq(ParseError(ParseError.Pattern.Label("true"), 0)) =>

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
        case Seq(ParseError(ParseError.Pattern.Token('('), 0)) =>
      test("missingParenthesisClose") - assertErrors(parser, "(true"):
        case Seq(ParseError(ParseError.Pattern.Token(')'), 5)) =>
      test("missingTrue") - assertErrors(parser, "()"):
        case Seq(ParseError(ParseError.Pattern.Label("true"), 1)) =>

    test("isSuccessful"):
      val parser: Parser[Char, Boolean] = Parser.isSuccessful(Parser.literal('a'))

      test("true") - assertSuccess(parser, "a")(true, 0)
      test("false"):
        test("unexpected") - assertSuccess(parser, "b")(false, 0)
        test("eof") - assertSuccess(parser, "")(false, 0)

    test("option"):
      val parser: Parser[Char, Option[Char]] = Parser.option(Parser.oneOf("ab"))

      test("success"):
        test - assertSuccess(parser, "a")(Some('a'))
        test - assertSuccess(parser, "b")(Some('b'))

      test("none") - assertSuccess(parser, "c")(None, 0)

    test("orError"):
      val parser: Parser[Char, Unit] = Parser.expect(Parser.literal('a'), "The letter a")

      test("success") - assertSuccess(parser, "a")(())
      test("failure"):
        test("unexpected") - assertErrors(parser, "b"):
          case Seq(ParseError(ParseError.Pattern.Label("The letter a"), 0)) =>

    test("commit"):
      val identifier: Parser[Char, String] = Parser.expect(Parser.regex("[a-zA-Z]+"), "identifier")
      val parser: Parser[Char, Expr] = Parser.expect(
        Parser.firstOf(
          Expr.Assign.apply.tupled(Parser.inOrder(identifier, Parser.literal('='), Parser.commit(identifier))),
          Expr.VarCall(identifier)
        ),
        "expr"
      )

      test("success"):
        test("varCall") - assertSuccess(parser, "x")(Expr.VarCall("x"))
        test("assign") - assertSuccess(parser, "x=y")(Expr.Assign("x", "y"))

      test("failure"):
        test("unexpectedExpr") - assertErrors(parser, "5"):
          case Seq(ParseError(Pattern.Label("expr"), 0)) =>
        
        test("malformedAssign") - assertErrors(parser, "x="):
          case Seq(ParseError(Pattern.Label("identifier"), 2)) =>

    test("not"):
      val parser: Parser[Char, Unit] = Parser.not(Parser.literal('a'))

      test("success"):
        test("notA") - assertSuccess(parser, "b")((), 0)
        test("eof") - assertSuccess(parser, "")((), 0)
      test("a") - assertErrors(parser, "a"):
        case Seq(ParseError(ParseError.Pattern.SomethingElse, 0)) =>

    test("andCheck"):
      val parser: Parser[Char, Char] = Parser.andCheck(
        Parser.next,
        Parser.literal("abc")
      )

      test("success") - assertSuccess(parser, "abc")('a', 1)
      test("failure") - assertErrors(parser, "a"):
        case Seq(ParseError(ParseError.Pattern.SomethingElse, 0)) =>

    test("skipUntil"):
      val parser: Parser[Char, Unit] = Parser.skipUntil(Parser.literal('b'))

      test("success") - assertSuccess(parser, "aaab")((), 3)
      test("onlyUntil") - assertSuccess(parser, "b")((), 0)
      test("eof") - assertErrors(parser, "aaa"):
        case Seq(ParseError(ParseError.Pattern.SomethingElse, 3)) =>
      test("untilEOF") - assertSuccess(Parser.skipUntil(Parser.eof), "aaa")(())

    test("repeat"):
      def parser(allowEmpty: Boolean): Parser[Char, List[Char]] = Parser.repeat(Parser.oneOf("ab"), allowEmpty = allowEmpty)
      test("one") - assertSuccess(parser(allowEmpty = true), "a")(List('a'))
      test("multiple") - assertSuccess(parser(allowEmpty = true), "ab")(List('a', 'b'))
      test("none"):
        test("allowEmpty") - assertSuccess(parser(allowEmpty = true), "")(Nil)
        test("forbidEmpty") - assertErrors(parser(allowEmpty = false), ""):
          case Seq(ParseError(_, 0)) =>

    test("repeatUntil"):
      def parser(allowEmpty: Boolean): Parser[Char, List[Char]] = Parser.repeatUntil(Parser.oneOf("ab"), Parser.literal("END"), allowEmpty = allowEmpty)

      test("success") - assertSuccess(parser(allowEmpty = true), "abaaEND")(List('a', 'b', 'a', 'a'), 4)
      test("onlyUntil"):
        test("allowEmpty") - assertSuccess(parser(allowEmpty = true), "END")(Nil, 0)
        test("forbidEmpty") - assertErrors(parser(allowEmpty = false), "END"):
          case Seq(ParseError(ParseError.Pattern.Label(_), 0)) =>
      test("unexpectedElement") - assertErrors(parser(allowEmpty = true), "abacEND"):
        case Seq(ParseError(ParseError.Pattern.Label(_), 3)) =>
      test("noUntil") - assertErrors(parser(allowEmpty = true), "abaa"):
        case Seq(ParseError(ParseError.Pattern.SomethingElse, 4)) =>

    test("repeatDiscard"):
      val parser: Parser[Char, Unit] = Parser.repeatDiscard(Parser.literal("ab"))

      test("success") - assertSuccess(parser, "ababab")(())
      test("untilUnexpected"):
        assertSuccess(parser, "ababc")((), 4)
        assertSuccess(parser, "ababa")((), 4)
      test("onlyUnexpected") - assertSuccess(parser, "c")((), 0)
      test("eof") - assertSuccess(parser, "")(())

    test("separatedBy"):
      val parser: Parser[Char, List[Char]] = Parser.separatedBy(
        Parser.oneOf("ab"),
        Parser.literal(',')
      )

      test("noElement") - assertSuccess(parser, "")(Nil)
      test("single") - assertSuccess(parser, "a")(List('a'))
      test("multiple") - assertSuccess(parser, "a,b,a")(List('a', 'b', 'a'))
      test("untilError") - assertSuccess(parser, "a,b,a]")(List('a', 'b', 'a'), 5)

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
        case Seq(ParseError(ParseError.Pattern.Label(_), 2)) =>
      test("noUntil") - assertErrors(parser, "a,b"):
        case Seq(ParseError(ParseError.Pattern.Token(','), 3)) =>

    test("separatedByReduce"):
      val intParser: Parser[Char, Int] = Parser.expect(
        Parser
          .regex("[0-9]+")
          .toIntOption
          .getOrElse(Parser.backtrack),
        "int"
      )

      val operatorParser: Parser[Char, (Int, Int) => Int] = Parser.firstOf(
        Parser.as(Parser.literal('+'), _ + _),
        Parser.as(Parser.literal('-'), _ - _)
      )

      val parser: Parser[Char, Int] = Parser.separatedByReduce(intParser, operatorParser)

      test("single") - assertSuccess(parser, "5")(5)
      test("multiple") - assertSuccess(parser, "1+2+3+4+5")(15)
      test("untilError") - assertSuccess(parser, "1+2+3+4+5/2")(15, 9)
      test("noElement") - assertErrors(parser, ""):
        case Seq(ParseError(ParseError.Pattern.Label("int"), 0)) =>
      test("firstInvalid") - assertErrors(parser, "a"):
        case Seq(ParseError(ParseError.Pattern.Label("int"), 0)) =>
