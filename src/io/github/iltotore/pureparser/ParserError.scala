package io.github.iltotore.pureparser

case class ParseError[+I](expected: ParseError.Pattern[I], at: Int) derives CanEqual

object ParseError:

  enum Pattern[+I] derives CanEqual:
    case Token(token: I)
    case Label(label: String)
    case SomethingElse
    case EOF

  def apply[I](token: I, at: Int): ParseError[I] = ParseError(Pattern.Token(token), at)
  def apply[I](label: String, at: Int): ParseError[I] = ParseError(Pattern.Label(label), at)