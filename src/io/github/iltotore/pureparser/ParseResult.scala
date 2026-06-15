package io.github.iltotore.pureparser

/**
  * The result of a parser. Unlike [[Either]], a [[ParseResult]] can contain both an output value and errors,
  * typically in the case of a recovered AST.
  *
  * @tparam A the output type.
  * @param output the value outputted by the parser. The [[Option]] is empty only if no AST could be recovered.
  * @param errors the emitted parsing errors. No error means that the parsing was successful.
  * @param endPosition the position the parser ended at. It is often supposed to be at the end of the token list (EOF).
  */
case class ParseResult[A](output: Option[A], errors: Seq[ParseError], endPosition: Int)