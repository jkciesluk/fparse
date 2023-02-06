package fparse.string

import fparse.regex.FromRegexParser
import fparse.reader.InputReader

object RegexParser extends StringParsers[Option] {
  def fromRegex(inp: Input): Parser[String] =
    Parser { inp0 =>
      FromRegexParser.fromRegex(inp)(inp0) match
        case FromRegexParser.Success(res: List[(String, Input)]) =>
          Success(res.headOption)
        case FromRegexParser.Error(msg, inp)   => Error(msg, inp)
        case FromRegexParser.Failure(err, inp) => Failure(err, inp)
    }

  def fromRegex(inp: String): Parser[String] = fromRegex(makeInput(inp))

  def fromRegexMax(inp: Input): Parser[String] =
    Parser { inp0 =>
      FromRegexParser.fromRegex(inp)(inp0) match
        case FromRegexParser.Success(res: List[(String, Input)]) =>
          res match
            case head :: next =>
              val res0 =
                next.foldLeft(head)((max, e) =>
                  if max._2.pos.compare(e._2.pos) >= 0 then max
                  else e
                )
              Success(Some(res0))
            case Nil => Error("Impossible", None)

        case FromRegexParser.Error(msg, inp)   => Error(msg, inp)
        case FromRegexParser.Failure(err, inp) => Failure(err, inp)
    }

  def fromRegexMax(inp: String): Parser[String] = fromRegexMax(makeInput(inp))
}
