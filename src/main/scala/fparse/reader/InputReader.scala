package fparse.reader

import fparse.positions.Position
import cats.kernel.Order
import java.util.Comparator

trait InputReader[+T] {
  def first: T
  def next: InputReader[T]
  def pos: Position
  def isEmpty: Boolean
  def compare[U](that: InputReader[U]): Int = pos.compare(that.pos)

}

object InputReader {
  given ord: Order[InputReader[_]] =
    new Order[InputReader[_]] {
      def compare(x: InputReader[?], y: InputReader[?]): Int = x.compare(y)
    }

  given ordering: Ordering[InputReader[_]] =
    Ordering.fromLessThan((ir1, ir2) => ir1.pos.compare(ir2.pos) < 0)

}
