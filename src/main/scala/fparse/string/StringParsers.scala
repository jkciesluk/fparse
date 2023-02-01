package fparse.string

import fparse.reader.CharReader
import fparse.Parsers

trait StringParsers extends Parsers {
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
  def int(i: Int): Parser[Int] =
    int.filter(_ == i)

  def double: Parser[Double] = for {
    n <- repeatedOne(digit)
    _ <- single('.')
    m <- repeatedOne(digit)
  } yield (n.mkString + '.' + m.mkString).toDouble

  override def whitespace: Parser[List[Char]] = repeatedOne(anyOf(whitespaces))

  override def optWhitespace: Parser[List[Char]] = repeated(anyOf(whitespaces))

  override def withOptWhitespace[A](fa: Parser[A]) = between(optWhitespace, fa)

}

