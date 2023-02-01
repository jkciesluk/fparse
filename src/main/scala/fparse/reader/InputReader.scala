package fparse.reader

import fparse.positions.Position

trait InputReader[+T] {
  def first: T
  def next: InputReader[T]
  def pos: Position
  def isEmpty: Boolean
}