package io.github.iltotore.pureparser.web

enum Msg derives CanEqual:
  case NoOp
  case SetInput(value: String)