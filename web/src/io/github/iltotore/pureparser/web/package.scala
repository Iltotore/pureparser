package io.github.iltotore.pureparser.web

import scala.quoted.*
import scala.util.Using
import scala.io.Source
import java.io.FileNotFoundException

inline def loadCompileTime(inline path: String): String = ${loadCompileTimeImpl('path)}

// Unforunately, SJS linking tasks do not support having resources or compile-time resources
// as they lead to an "illegal classpath" error. (issue on SJS or Mill side?)
def loadCompileTimeImpl(path: Expr[String])(using Quotes): Expr[String] =
  try
    Expr(Using.resource(Source.fromFile(path.valueOrAbort))(_.mkString))
  catch case _: FileNotFoundException => Expr("")