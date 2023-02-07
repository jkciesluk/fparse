package fparse

import fparse.string.StringParsers

object JSONParser extends StringParsers[Option] {

  trait JSON
  case class JArray(elems: List[JSON]) extends JSON
  case class JObject(fields: List[JField]) extends JSON
  case class JField(name: String, value: JSON) extends JSON
  case class JString(value: String) extends JSON
  case class JInt(value: Int) extends JSON

  def jIntParser: Parser[JInt] = int.map(JInt(_))
  def stringParser: Parser[List[Char]] =
    between(Parser('"'), takeWhile(_ != '"'))
  def jStringParser: Parser[JString] =
    stringParser.map(str => JString(str.mkString))

  private def sepParser(c: Char) =
    between(optWhitespace, Parser(c))

  def jFieldParser: Parser[JField] = for
    name <- stringParser
    _ <- sepParser(':')
    value <- jsonParser
  yield JField(name.mkString, value)

  def jArrayParser: Parser[JArray] = for {
    _ <- withOptWhitespace(Parser('['))
    elems <- separated(jsonParser, sepParser(','))
    _ <- withOptWhitespace(Parser(']'))
  } yield JArray(elems)

  def jObjectParser: Parser[JObject] = for {
    _ <- withOptWhitespace(Parser('{'))
    fields <- separated(jFieldParser, sepParser(','))
    _ <- withOptWhitespace(Parser('}'))
  } yield JObject(fields)

  def jsonParser: Parser[JSON] =
    (jIntParser <|> jStringParser <|> jFieldParser <|> jObjectParser <|> jArrayParser).withOptWhitespace

}
