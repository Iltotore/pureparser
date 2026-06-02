package io.github.iltotore.pureparser

case class ParseResult[A](output: Option[A], errors: Seq[ParseError], endPosition: Int)