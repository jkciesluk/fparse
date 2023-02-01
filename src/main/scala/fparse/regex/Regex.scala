package fparse.regex

// TODO: ADD GROUP CAPTURING
sealed trait Regex
sealed trait QRegex extends Regex
sealed trait URegex extends Regex
case class Group(r: Regex) extends URegex
case class AnyOf(xs: List[Regex]) extends URegex
case class AnyExcept(xs: Regex) extends URegex
case class Multiple(m: More, lzy: Boolean = false) extends QRegex
// case class Char(c: Single) extends Regex
case class Or(r1: Regex, r2: Regex) extends URegex
case class Cat[A <: Regex](r1: Regex, r2: Regex) extends URegex
case object End extends Regex
case object Start extends Regex
  
sealed trait More extends QRegex
case class Star(r: URegex) extends More
case class Plus(r: URegex) extends More
case class Opt(r: URegex) extends More
case class Times(n: Int, r: URegex) extends More
case class TimesRange(from: Int, to: Option[Int], r: URegex) extends More

sealed trait Single extends URegex
case object Dot extends Single
case class Ch(c: scala.Char) extends Single
// case class String(s: String) extends Single
// case class Hex(h: String) extends Single
sealed trait CharClass extends Single
case class NegatedClass(c: CharClass) extends Single

case object Digit extends CharClass
case object Word extends CharClass
case object Whitespace extends CharClass

object Regex {
  //TODO: add some assertions
  def makeRange(a: Ch, b: Ch) =
    (a.c to b.c).toList.map(Ch(_))
}