package fparse.positions

case class SeqPosition(offset: Int) extends Position {
  override def toString(): String = s"($offset)"
  def next(): SeqPosition = SeqPosition(offset + 1)
}