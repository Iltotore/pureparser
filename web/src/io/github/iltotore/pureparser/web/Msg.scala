package io.github.iltotore.pureparser.web

enum Msg derives CanEqual:
  case NoOp
  case SetExample(example: Example, input: String)
  case SetInput(value: String)