package io.github.iltotore.pureparser.util

/**
  * A dummy implicit, used for clause interleaving between two type parameter lists,
  * leading to better type inference. 
  */
type Dummy = Dummy.type

object Dummy:
  given Dummy = Dummy