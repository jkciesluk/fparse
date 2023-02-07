package fparse.example

import fparse.positions.StringPosition

sealed trait Token
enum Keyword extends Token:
  case NOT, TRUE, FALSE, BOOL, ELSE, IF, LENGTH, RETURN, TYPE, USE, WHILE, OR,
    AND, INT

enum Tk extends Token:
  case PLUS, MINUS, TIMES, DIV, REM
  case LPAREN, RPAREN, DOT, COMMA, COLON, SEMICOLON, LCBRACE, RCBRACE,
    LSBRACKET, RSBRACKET
  case UNFOLD, LE, GE, EQ, NEQ, LT, GT, ASSIGN

case class Char(c: scala.Char) extends Token
case class Quote(s: String) extends Token
case class Ident(id: String) extends Token
case object EOF extends Token
case class Number(n: Int) extends Token

case class PositionedToken(tk: Token, pos: StringPosition)
