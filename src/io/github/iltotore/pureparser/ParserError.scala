package io.github.iltotore.pureparser

enum ParseError:
  case EOF
  case UnexpectedToken(expected: String, at: Int)