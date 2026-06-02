package io.github.iltotore.pureparser

import utest.*

def assertSuccess[A](parser: Parser[Char, A], input: String)(expectedResult: A, expectedPosition: Int = input.length)(using CanEqual[A, A]): Unit =
  val result = Parser(input)(parser)
  assert(result.output.exists(_ == expectedResult), result.endPosition == expectedPosition)

def assertErrors(parser: Parser[Char, Any], input: String)(matchError: PartialFunction[Seq[ParseError], Unit]): Unit =
  val result = Parser(input)(parser)
  assertMatch(result.errors)(matchError.asInstanceOf[PartialFunction[Any, Unit]])
