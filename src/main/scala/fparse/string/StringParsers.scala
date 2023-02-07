package fparse.string

import fparse.reader.CharReader
import fparse.Parsers
import cats.Monad
import cats.Alternative

trait StringParsers[F[_]: Monad: Alternative] extends Parsers[F] {
  type Elem = Char
  // type Input = CharReader
  val whitespaces = Set(' ', '\t', '\r', '\n')
  def makeInput(str: String) = CharReader.fromString(str)

  // given Conversion[Char, Parser[Char]] = Parser.accept(_)
  // given Conversion[String, Parser[String]] with
  // def apply(str: String): Parser[String] = Parser.sequence(str.toList.map(Parser.accept)).map(_.mkString)

  def digit: Parser[Char] = anyOf("0123456789".toList)

  def natural: Parser[Int] = {
    repeatedOne(digit).map(_.mkString.toInt)
  }

  def int: Parser[Int] = {
    (opt(Parser('-')) ~ natural).map((minus, n) => minus.fold(n)(_ => -n))
  }
  def int(i: Int): Parser[Int] = ???
  // int.filter(_ == i)

  def double: Parser[Double] = for {
    n <- repeatedOne(digit)
    _ <- accept('.')
    m <- repeatedOne(digit)
  } yield (n.mkString + '.' + m.mkString).toDouble

  def word: Parser[String] =
    takeWhile(!whitespaces(_)).map(_.mkString)

  def accept(w: String): Parser[String] =
    sequence(w.map(accept).toList).map(_.mkString)

  def line: Parser[String] =
    takeWhile(e => e != '\r' && e != '\n').map(_.mkString)

  override def whitespace: Parser[List[Char]] = repeatedOne(anyOf(whitespaces))
  def skipWhitespace: Parser[Unit] = repeatedOne(anyOf(whitespaces)).void

  override def optWhitespace: Parser[List[Char]] = repeated(anyOf(whitespaces))

  override def withOptWhitespace[A](fa: Parser[A]) =
    between(optWhitespace, fa)

  def escapeChars: Set[Char] = Set.empty
  def escapeChar = for {
    _ <- accept('\\')
    c <- anyOf(escapeChars)
  } yield c

}
