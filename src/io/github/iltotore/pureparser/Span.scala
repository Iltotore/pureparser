package io.github.iltotore.pureparser

case class Span(start: Int, end: Int) derives CanEqual:

  def merge(other: Span): Span = Span(math.min(this.start, other.start), math.max(this.end, other.end))