package io.github.iltotore.pureparser.util

import scala.compiletime.erasedValue
import scala.compiletime.summonInline

@FunctionalInterface
trait Zip[A, B]:

  type Zipped

  def zip(a: A, b: B): Zipped

object Zip extends ZipLowPriority:

  type All[T <: Tuple] = T match
    case head *: EmptyTuple => head
    case head *: tailToZip => (head, All[tailToZip]) match
        case (Unit, tail)   => tail
        case (_, Unit)      => head
        case (Tuple, Tuple) => Tuple.Concat[head, All[tailToZip]]
        case (Tuple, tail)  => Tuple.Append[head, tail]
        case (head, Tuple)  => head *: All[tailToZip]
        case (head, tail)   => (head, tail)

  inline def foldingAll[T <: Tuple]: List[Zip[Any, Any]] = inline erasedValue[T] match
    case _: EmptyTuple     => Nil
    case _: (head *: EmptyTuple) => List(summonInline[Zip[head, Unit]].asInstanceOf[Zip[Any, Any]])
    case _: (head *: tail) => summonInline[Zip[head, All[tail]]].asInstanceOf[Zip[Any, Any]] :: foldingAll[tail]

  given Zip[Unit, Unit] with
    override type Zipped = Unit
    override def zip(a: Unit, b: Unit): Zipped = ()

  given [A]: Zip[A, Unit] with
    override type Zipped = A
    override def zip(a: A, b: Unit): Zipped = a

  given [B]: Zip[Unit, B] with
    override type Zipped = B
    override def zip(a: Unit, b: B): Zipped = b

trait ZipLowPriority:

  given [TA <: Tuple, TB <: Tuple]: Zip[TA, TB] with
    override type Zipped = Tuple.Concat[TA, TB]
    override def zip(a: TA, b: TB): Zipped = a ++ b

  given [TA <: Tuple, B]: Zip[TA, B] with
    override type Zipped = Tuple.Append[TA, B]
    override def zip(a: TA, b: B): Zipped = a :* b

  given [A, TB <: Tuple]: Zip[A, TB] with
    override type Zipped = A *: TB
    override def zip(a: A, b: TB): Zipped = a *: b

  given [A, B]: Zip[A, B] with
    override type Zipped = (A, B)
    override def zip(a: A, b: B): Zipped = (a, b)
