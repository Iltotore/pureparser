package io.github.iltotore.pureparser

enum ParseError derives CanEqual:
  case EOF
  case UnexpectedToken(expected: String, at: Int)