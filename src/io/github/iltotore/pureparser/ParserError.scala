package io.github.iltotore.pureparser

/**
  * A parsing error.
  */
enum ParseError derives CanEqual:

  /**
    * Unexpected end of file.
    */
  case EOF

  /**
    * Unexpected token.
    *
    * @param expected the expected token(s) or pattern(s).
    * @param at where the unexpected token was encountered. If the tokens carry their [[Span]], you can use the one of the token at [[at]].
    */
  case UnexpectedToken(expected: String, at: Int)