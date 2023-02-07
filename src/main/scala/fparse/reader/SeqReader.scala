package fparse.reader

import fparse.positions.SeqPosition

class SeqReader[A](inp: Seq[A], offset: SeqPosition) extends InputReader[A] {

  override def first: A = inp.head

  override def next: InputReader[A] =
    if isEmpty then this else SeqReader(inp.tail, offset.next())

  override def pos: SeqPosition = offset

  override def isEmpty: Boolean = inp.isEmpty

}

object SeqReader {
  def fromList[A](xs: List[A]): SeqReader[A] = SeqReader(xs, SeqPosition(0))
}
