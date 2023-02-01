package fparse.positions


case class StringPosition(offset: Int, line: Int, column: Int) extends Position {
  override def toString(): String = s"($line, $column)"
  def nextLine() = StringPosition(offset + 1, line + 1, 0)
  def nextColumn() = StringPosition(offset + 1, line, column + 1)
}