package io.github.iltotore.pureparser

/**
  * A span between two positions in the input list, typically used to track the position of each parsed node.
  *
  * @param start the start of this [[Span]] (inclusive).
  * @param end the end of this [[Span]] (exclusive).
  * @note it is always assumed that `start < end`. 
  */
case class Span(start: Int, end: Int) derives CanEqual:

  /**
    * Merge this [[Span]] with another.
    *
    * @param other the other [[Span]] to combine with this one.
    * @return a [[Span]] covering both merged ones. Since [[Span]]s cannot be discontinuous, anything between the two merged [[Span]]s
    * is also covered by the merge result. 
    */
  def merge(other: Span): Span = Span(math.min(this.start, other.start), math.max(this.end, other.end))

  /**
    * The size of this span.
    *
    * @return the difference between [[end]] and [[start]].
    */
  def size: Int = this.end - this.start