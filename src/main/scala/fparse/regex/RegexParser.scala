package fparse.regex

import fparse.string.StringParsers
import fparse.Parsers

object RegexAstParser extends StringParsers[Option] {
  private val parseStart: Parser[Regex] = Parser('^').map(_ => Start)
  private val parseEnd: Parser[Regex] = Parser('$').map(_ => End)

  private val escapeChars = Set('$', '\\', '.', '+', '*', '?', '^', '|', '/', ']', '[', '(', ')', '{', '}')
  private val parseEscapeChar = for {
    _ <- Parser('\\')
    c <- anyOf(escapeChars)
  } yield Ch(c)

  private val parseWhitespace = for {
    _ <- Parser('\\')
    c <- anyOf(Set('t', 'n', 'r'))
  } yield {
    c match
      case 't' => Ch('\t')
      case 'r' => Ch('\r')
      case 'n' => Ch('\n')
    
  }
  private val parseCh: Parser[Ch] = 
    anyExcept(escapeChars).map(Ch(_)) <|> parseEscapeChar <|> parseWhitespace
  private val parseDot: Parser[Single] = Parser('.').map(_ => Dot)
  private val charClasses = Set('w', 'W', 'd', 'D', 's', 'S')
  private val parseCharClass: Parser[Single] = for {
    _ <- Parser('\\')
    cls <- anyOf(charClasses)
  } yield {
    cls match
      case 'd' => Digit
      case 'D' => NegatedClass(Digit)
      case 'w' => Word
      case 'W' => NegatedClass(Word)
      case 's' => Whitespace
      case 'S' => NegatedClass(Whitespace)
  }

  private val parseSingle: Parser[Single] =
    oneOf(List(parseDot, parseCharClass, parseEscapeChar, parseWhitespace, parseCh))


  private val parseGroup: Parser[Group] = 
    for {
      _ <- Parser('(')
      r <- parseRegex
      _ <- Parser(')')
    } yield Group(r)

  private val parseAnyOf: Parser[URegex] = {
    val range: Parser[List[Single]] =
      val pair: Parser[(Ch, Ch)] = ((parseCh <* Parser('-')) ~ parseCh)
      pair.map(Regex.makeRange(_, _))
    val anyOf: Parser[List[Single]] = (anyExcept(']') <|> (Parser('\\') *> Parser(']'))).map(Ch(_)).repeated
    for {
      _ <- Parser('[')
      optNeg <- Parser('^').?
      middle <- range <|> anyOf
      _ <- Parser(']')
    } yield {
      if optNeg.isEmpty then AnyOf(middle)
      else AnyExcept(AnyOf(middle))
    }
  }

  // private def parseOr: Parser[URegex] = for {
  //     left <- parseRegex
  //     _ <- Parser('|')
  //     right <- parseRegex
  // } yield Or(left, right)

  private def parseUnquantified: Parser[URegex] = 
    oneOf(List(parseGroup, parseAnyOf, parseSingle))
  
  
  private def parseMore: Parser[More] = 
    val parsePlus: Parser[Plus] = (parseUnquantified <* Parser('+')).map(Plus(_))
    val parseStar: Parser[Star] = (parseUnquantified <* Parser('*')).map(Star(_))
    val parseOpt:  Parser[Opt]  = (parseUnquantified <* Parser('?')).map(Opt(_))

    val parseTimes: Parser[Times] = for {
      regex <- parseUnquantified
      _ <- Parser('{')
      n <- natural
      _ <- Parser('{')
    } yield Times(n, regex)
    val parseTimesRange = for {
      regex <- parseUnquantified
      _ <- Parser('{')
      n <- natural
      mOpt <- (Parser(',') *> natural).? 
      _ <- Parser('{')
    } yield TimesRange(n, mOpt, regex)
    parsePlus <|> parseStar <|> parseTimes <|> parseTimesRange

  private def parseMultiple: Parser[Multiple] = for{
    more <- parseMore
    optLzy <- Parser('?').?
  } yield Multiple(more, optLzy.isEmpty)

      

  def parseRegex: Parser[Regex] = 
    val simple = oneOf(List(parseMultiple, parseUnquantified, parseStart, parseEnd))
    val parserOr = for {
      r1 <- simple
      _ <- Parser('|')
      r2 <- parseRegex
    } yield Or(r1, r2)
    (parserOr <|> simple).repeatedOne.map{
      case head :: next => next.foldLeft(head)((acc, reg) => Cat(acc, reg))
      case Nil => End
    }
}
