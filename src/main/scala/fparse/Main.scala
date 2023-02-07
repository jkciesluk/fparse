package fparse

import cats.implicits._

import fparse.regex.RegexAstParser
import fparse.string.RegexParser
import fparse.example.Lexer
import fparse.example.{Parser => ZiParser}

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

  val regexInput = "^(ab)(d|c+?)[AEIOU]*$"

  val ziInput = s"""|f() {
                    |  a:int[] = {1,2,3,}
                    |  b:bool[][2]
                    |  c:int[1][2][3]
                    |  d:bool [ ]
                    |  bar({})
                    |}
                    |""".stripMargin
  val ziTokens = Lexer.getTokens(ziInput)
  val regRes =
    RegexParser.fromRegexMax(regexInput).run(RegexParser.makeInput("abcccAI"))
  val json = JSONParser.jsonParser.run(JSONParser.makeInput(input))
}
