package io.github.iltotore.pureparser.web

import scala.compiletime.*
import scala.deriving.Mirror
import io.github.iltotore.pureparser.Parser
import scala.reflect.Enum
import io.github.iltotore.pureparser.example.json.Json
import io.github.iltotore.pureparser.Span

trait ToTree[-A]:

  def toTree(name: String, value: A): Tree

object ToTree extends ToTreeLowPriority:

  def apply[A](name: String, value: A)(using toTree: ToTree[A]): Tree = toTree.toTree(name, value)

  inline def summonInstances[T, Elems <: Tuple]: List[ToTree[?]] =
    inline erasedValue[Elems] match
      case _: (elem *: elems) => deriveOrSummon[T, elem] :: summonInstances[T, elems]
      case _: EmptyTuple => Nil

  inline def deriveOrSummon[T, Elem]: ToTree[Elem] =
    inline erasedValue[Elem] match
      case _: T => deriveRec[T, Elem]
      case _    => summonInline[ToTree[Elem]]

  inline def deriveRec[T, Elem]: ToTree[Elem] =
    inline erasedValue[T] match
      case _: Elem => error("infinite recursive derivation")
      case _       => ToTree.derived[Elem](using summonInline[Mirror.Of[Elem]])

  def fromProduct[A <: Product](nodeName: String, elements: => List[ToTree[Any]]): ToTree[A] =
    (name, value) => Tree.Node(
      s"$name: $nodeName",
      elements
        .zipWithIndex
        .map((toTree, i) => toTree.toTree(value.productElementName(i), value.productElement(i)))
    )

  def fromSum[A](m: Mirror.SumOf[A], elements: => List[ToTree[Any]]): ToTree[A] =
    (name, value) => elements(m.ordinal(value)).toTree(name, value)

  inline def derived[A](using m: Mirror.Of[A]): ToTree[A] =
    val nodeName = constValue[m.MirroredLabel]
    lazy val elemInstances = summonInstances[A, m.MirroredElemTypes].asInstanceOf[List[ToTree[Any]]]
    inline m match
      case s: Mirror.SumOf[A]     => fromSum(s, elemInstances)
      case p: Mirror.ProductOf[A] => fromProduct[A & Product](nodeName, elemInstances).asInstanceOf[ToTree[A]]

  given [A](using ToTree[A]): ToTree[List[A]] = (name, values) =>
    Tree.Node(name, values.zipWithIndex.map((value, i) => ToTree(s"Element $i", value)))

  given [A](using ToTree[A]): ToTree[Map[String, A]] = (name, fields) =>
    Tree.Node(name, fields.map((fieldName, fieldValue) => ToTree(fieldName, fieldValue)).toList)

  given ToTree[Span] = ToTree.derived

trait ToTreeLowPriority:

  given [A]: ToTree[A] = (name, value) => Tree.Leaf(name, pprint(value).plainText)