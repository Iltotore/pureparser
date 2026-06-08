package io.github.iltotore.pureparser.web

enum Tree:
  case Leaf(name: String, value: String)
  case Node(name: String, children: List[Tree])