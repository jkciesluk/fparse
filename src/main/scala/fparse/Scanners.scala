package fparse

import fparse.Parsers
import cats.Monad
import cats.Alternative
import fparse.reader.SeqReader

trait Scanners[A, F[_]: Monad: Alternative] extends Parsers[F] {
  type Elem = A
  def makeInput(xs: List[A]): SeqReader[Elem] = SeqReader.fromList(xs)
}
