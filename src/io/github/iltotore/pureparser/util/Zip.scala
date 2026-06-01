package io.github.iltotore.pureparser.util

@FunctionalInterface
trait Zip[A, B]:

  type Zipped

  def zip(a: A, b: B): Zipped

object Zip:

  given Zip[Unit, Unit] with
    override type Zipped = Unit
    override def zip(a: Unit, b: Unit): Zipped = ()

  given [A]: Zip[A, Unit] with
    override type Zipped = A
    override def zip(a: A, b: Unit): Zipped = a

  given [B]: Zip[Unit, B] with
    override type Zipped = B
    override def zip(a: Unit, b: B): Zipped = b

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