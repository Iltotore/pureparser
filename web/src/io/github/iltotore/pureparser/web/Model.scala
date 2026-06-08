package io.github.iltotore.pureparser.web

import io.github.iltotore.pureparser.ParseResult

case class Model(examples: List[Example], selectedExample: Example, input: String, result: ParseResult[Tree])

object Model:

  val default: Model = Model(
    examples = Example.defaults,
    selectedExample = Example.defaultSelection,
    input = "",
    result = ParseResult(None, Seq.empty, 0)
  )