package fparse.positions

trait Position {
  def offset: Int
  def compare(that: Position): Int = this.offset.compareTo(that.offset)
}
