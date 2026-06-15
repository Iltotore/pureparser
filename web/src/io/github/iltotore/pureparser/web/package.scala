package io.github.iltotore.pureparser.web

import scala.quoted.*
import scala.util.Using
import scala.io.Source
import java.io.FileNotFoundException

inline def loadCompileTime(inline path: String): String = ${loadCompileTimeImpl('path)}

// Unforunately, SJS linking tasks do not support having resources or compile-time resources
// as they lead to an "illegal classpath" error. (issue on SJS or Mill side?)
def loadCompileTimeImpl(path: Expr[String])(using Quotes): Expr[String] =
  val pathValue = s"${sys.props.getOrElse("examples.dir", "")}/${path.valueOrAbort}"
  try
    Expr(Using.resource(Source.fromFile(pathValue))(_.mkString))
  catch case _: FileNotFoundException =>
    quotes.reflect.report.errorAndAbort(s"File not found: $pathValue.")