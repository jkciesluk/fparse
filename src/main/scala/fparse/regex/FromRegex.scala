package fparse.regex

import fparse.string.StringParsers

object FromRegexParser extends StringParsers {
  def buildParser(r: Regex): Parser[String] =
    r match
      case Group(r)         => buildParser(r)
      case AnyOf(xs)        => oneOf(xs.map(buildParser))
      case AnyExcept(r)     => (buildParser(r) *> Rip) <|> elem.map(_.toString())
      case Multiple(m, lzy) => buildParser(m) // niech lzy cos robi
      case Or(r1, r2)       => buildParser(r1) <|> buildParser(r2)
      case Cat(r1, r2)      => (buildParser(r1) ~ buildParser(r2)).map(_ + _)
      case End              => eof.map(_ => "")
      case Start            => pure("") // chwilowo pure, do poprawy?
      case Dot              => anyExcept('\n').map(_.toString())
      case Ch(c)            => accept(c).map(_.toString())
      case NegatedClass(c)  => (buildParser(c) *> Rip) <|> elem.map(_.toString()) // brakuje mi niezawracajacego orElse
      case Digit            => digit.map(_.toString())
      case Word             => ??? // dodac takie cos w StringParsers
      case Whitespace       => anyOf(whitespaces).map(_.toString())
      case Star(r)          => repeated(buildParser(r)).map(_.mkString)
      case Plus(r)          => repeatedOne(buildParser(r)).map(_.mkString)
      case Opt(r)           => opt(buildParser(r)).map(_.getOrElse(""))
      case Times(n, r)      => ??? // potrzebuje dodac repeated z n
      case TimesRange(from, to, r) => ??? // potrzebuje dodac repeated z n
}
