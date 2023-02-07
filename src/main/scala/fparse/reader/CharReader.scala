package fparse.reader

import java.io.File
import scala.io.Source

import fparse.positions.StringPosition
class CharReader(source: String, val pos: StringPosition)
    extends InputReader[Char] {
  val offset = pos.offset
  val sourceLength = source.length()
  def isEmpty = sourceLength <= offset
  def first: Char = source.charAt(offset)
  def next = if isEmpty then this
  else if first == '\n' then moveLine()
  else moveColumn()

  override def toString(): String =
    s"Pos: $pos" + '\n' + source.drop(offset).take(10)
  private def withPos(p: StringPosition) = CharReader(source, p)
  private def moveLine(): CharReader = withPos(pos.nextLine())
  private def moveColumn(): CharReader = withPos(pos.nextColumn())
}

object CharReader {
  val startPos = StringPosition(0, 0, 0)
  def fromString(str: String) = CharReader(str, startPos)
  def fromFile(f: File) = CharReader(Source.fromFile(f).mkString, startPos)
}
