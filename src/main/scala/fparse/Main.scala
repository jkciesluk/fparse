package fparse

import cats.implicits._

import fparse.string.StringParsers
import fparse.regex.RegexAstParser
import fparse.string.RegexParser

object JParser extends StringParsers[Option] {

  trait JSON
  case class JArray(elems: List[JSON]) extends JSON
  case class JObject(fields: List[JField]) extends JSON
  case class JField(name: String, value: JSON) extends JSON
  case class JString(value: String) extends JSON
  case class JInt(value: Int) extends JSON

  def jIntParser: Parser[JInt] = int.map(JInt(_))
  def stringParser: Parser[List[Char]] =
    between(Parser('"'), takeWhile(_ != '"'))
  def jStringParser: Parser[JString] = stringParser.map(str => JString(str.mkString))

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

object Main extends App {
  val input = """|[
                 |	{
                 |		"id": "0001",
                 |		"type": "donut",
                 |		"name": "Cake",
                 |		"ppu": 55,
                 |		"batters":
                 |			{
                 |				"batter":
                 |					[
                 |						{ "id": "1001", "type": "Regular" },
                 |						{ "id": "1002", "type": "Chocolate" },
                 |						{ "id": "1003", "type": "Blueberry" },
                 |						{ "id": "1004", "type": "Devil's Food" }
                 |					]
                 |			},
                 |		"topping":
                 |			[
                 |				{ "id": "5001", "type": "None" },
                 |				{ "id": "5002", "type": "Glazed" },
                 |				{ "id": "5005", "type": "Sugar" },
                 |				{ "id": "5007", "type": "Powdered Sugar" },
                 |				{ "id": "5006", "type": "Chocolate with Sprinkles" },
                 |				{ "id": "5003", "type": "Chocolate" },
                 |				{ "id": "5004", "type": "Maple" }
                 |			]
                 |	},
                 |	{
                 |		"id": "0002",
                 |		"type": "donut",
                 |		"name": "Raised",
                 |		"ppu": 55,
                 |		"batters":
                 |			{
                 |				"batter":
                 |					[
                 |						{ "id": "1001", "type": "Regular" }
                 |					]
                 |			},
                 |		"topping":
                 |			[
                 |				{ "id": "5001", "type": "None" },
                 |				{ "id": "5002", "type": "Glazed" },
                 |				{ "id": "5005", "type": "Sugar" },
                 |				{ "id": "5003", "type": "Chocolate" },
                 |				{ "id": "5004", "type": "Maple" }
                 |			]
                 |	},
                 |	{
                 |		"id": "0003",
                 |		"type": "donut",
                 |		"name": "Old Fashioned",
                 |		"ppu": 55,
                 |		"batters":
                 |			{
                 |				"batter":
                 |					[
                 |						{ "id": "1001", "type": "Regular" },
                 |						{ "id": "1002", "type": "Chocolate" }
                 |					]
                 |			},
                 |		"topping":
                 |			[
                 |				{ "id": "5001", "type": "None" },
                 |				{ "id": "5002", "type": "Glazed" },
                 |				{ "id": "5003", "type": "Chocolate" },
                 |				{ "id": "5004", "type": "Maple" }
                 |			]
                 |	}
                 |]
                 |""".stripMargin

  val input2 = "[100, 200]"

  val regexInput = "^(ab)|c+?|[AEIOU]*$"
  val regexp = RegexAstParser.makeInput("a+ba")
  pprint.log(RegexAstParser.parseRegex.run(RegexAstParser.makeInput(regexInput)))
  pprint.log(RegexParser.fromRegex(regexp)(RegexAstParser.makeInput("aabab")))
  // val json = JParser.jsonParser(JParser.makeInput(input))
  // pprint.log(json)
}
