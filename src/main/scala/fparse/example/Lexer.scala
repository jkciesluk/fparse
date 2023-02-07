package fparse.example

import fparse.string.StringParsers
import fparse.regex.FromRegexParser
import fparse.regex.Regex
import fparse.string.RegexParsers

object Lexer extends RegexParsers {
  private val keywordMap: Map[String, Keyword] = Map(
    "not" -> Keyword.NOT,
    "true" -> Keyword.TRUE,
    "false" -> Keyword.FALSE,
    "bool" -> Keyword.BOOL,
    "else" -> Keyword.ELSE,
    "if" -> Keyword.IF,
    "length" -> Keyword.LENGTH,
    "return" -> Keyword.RETURN,
    "type" -> Keyword.TYPE,
    "use" -> Keyword.USE,
    "while" -> Keyword.WHILE,
    "or" -> Keyword.OR,
    "and" -> Keyword.AND,
    "int" -> Keyword.INT
  )
  // private val parseKeyword: Parser[Keyword] =
  // val keywords = keywordMap.keys.map(accept(_).map(keywordMap(_))).toList
  // oneOf(keywords)

  private val parseTk =
    oneOf(
      List(
        accept('+').map(_ => Tk.PLUS),
        accept('-').map(_ => Tk.MINUS),
        accept('*').map(_ => Tk.TIMES),
        accept('/').map(_ => Tk.DIV),
        accept('(').map(_ => Tk.LPAREN),
        accept(')').map(_ => Tk.RPAREN),
        accept('.').map(_ => Tk.DOT),
        accept(',').map(_ => Tk.COMMA),
        accept(':').map(_ => Tk.COLON),
        accept(';').map(_ => Tk.SEMICOLON),
        accept('{').map(_ => Tk.LCBRACE),
        accept('}').map(_ => Tk.RCBRACE),
        accept('[').map(_ => Tk.LSBRACKET),
        accept(']').map(_ => Tk.RSBRACKET),
        accept('^').map(_ => Tk.UNFOLD),
        accept('%').map(_ => Tk.REM),
        accept("<=").map(_ => Tk.LE),
        accept(">=").map(_ => Tk.GE),
        accept("==").map(_ => Tk.EQ),
        accept("!=").map(_ => Tk.NEQ),
        accept('<').map(_ => Tk.LT),
        accept('>').map(_ => Tk.GT),
        accept('=').map(_ => Tk.ASSIGN)
      )
    )

  override val escapeChars: Set[scala.Char] =
    Set('n', 't', 'r', '"', '\'', '\\')
  private val charLiteral: String = "[ -[]|[\\]-~]"
  private val parseChar = for {
    _ <- accept('\'')
    c <- (escapeChar <|> fromRegex(charLiteral).map(_.charAt(0)))
    _ <- accept('\'')
  } yield Char(c)
  private val strLiteral: String = " |!|[ -[]|[\\]-~]"

  private val parseString = for {
    _ <- accept('"')
    str <- (escapeChar <|> fromRegex(strLiteral).map(_.charAt(0))).repeated
      .map(_.mkString)
    _ <- accept('"')
  } yield Quote(str)

  private val identifierRegex: String =
    "(_|[a-z]|[A-Z])(_|[A-Z]|[a-z]|[0-9]|')*" // trzeba bedzie zrobic tak jak w ocamlu, identifier to moze byc keyword
    // number parsowac z identifier.? i jak ident nonEmpty to odrzucac

  private val parseIdent: Parser[Token] =
    fromRegex(identifierRegex).map(ident =>
      keywordMap.get(ident) match
        case None        => Ident(ident)
        case Some(value) => value
    )
  private val parseNumber = for {
    n <- natural
    identOpt <- (parseIdent).? if identOpt.isEmpty
  } yield Number(n)

  private val parseToken: Parser[Token] =
    parseTk <|> parseNumber <|> parseIdent <|> parseString <|> parseChar

  private val tokenizer: Parser[List[Token]] =
    (optWhitespace *> (parseToken <* optWhitespace).repeated ~ eof.map(_ =>
      EOF
    )).map(_ :+ _)

  def tokenize(inp: Input): ParseResult[List[Token]] =
    tokenizer(inp)

  // def tokenize(inp: String): ParseResult[List[Token]] = tokenize(makeInput(inp))

  def getTokens(inp: Input): List[Token] = tokenize(inp) match
    case Success(res)      => res.get._1
    case Error(msg, inp)   => throw Exception(msg)
    case Failure(err, inp) => throw Exception("lexer failure")

  def getTokens(inp: String): List[Token] = getTokens(makeInput(inp))

}
