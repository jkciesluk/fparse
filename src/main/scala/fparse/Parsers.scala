package fparse

import cats.implicits._
// import cats.syntax.all._
import cats.Alternative
import cats.FunctorFilter
import cats.Monad
import cats.Applicative
import cats.Functor
import cats.Defer

import fparse.reader.InputReader

trait Parsers {
  type Elem
  type Input = InputReader[Elem]

  enum ParseError:
    case UnexpectedChar(e: Elem) extends ParseError
    case ExpectedInt extends ParseError
    case ExpectedEof extends ParseError
    case UnexpectedEof extends ParseError
    case Fail extends ParseError
    case Rip extends ParseError

  type ParseResult[A] = Either[ParseError, (A, Input)]

  type Label = String

  class Parser[+A](parse0: Input => ParseResult[A]) {

    def parse(inp: Input): ParseResult[A] = parse0(inp)
    def apply(inp: Input) = parse(inp)

    def ? : Parser[Option[A]] = 
      this.map(Some(_)) <|> pure(None)
    def map[B](fn: A => B): Parser[B] =
      Parser(inp => parse(inp).map((a, rest) => (fn(a), rest)))
    def ^^[B](fn: A => B): Parser[B] = map(fn)

    

    def filter(fn: A => Boolean) = Parser { inp =>
      parse(inp) match
        case r @ Right((a, _)) if fn(a) => r
        case _: Right[?, ?]             => Left(ParseError.Fail)
        case error                      => error
    }

    def mapFilter[B](fn: A => Option[B]): Parser[B] =
      val fb = map { a =>
        fn(a) match
          case None    => Left(ParseError.Fail)
          case Some(b) => Right(b)
      }
      select(fb)(fail)

    def void: Parser[Unit] =
      Parser(inp => parse(inp).map((_, rest) => ((), rest)))

    def ~[B](that: Parser[B]): Parser[(A, B)] =
      Parser(inp =>
        parse(inp) match
          case Left(error)      => Left(error)
          case Right((a, rest)) => that(rest).map((b, r) => ((a, b), r))
      )

    def andThen[B](that: Parser[B]): Parser[(A, B)] = this ~ that

    def flatMap[B](fn: A => Parser[B]): Parser[B] =
      Parser(inp =>
        parse(inp) match
          case Left(error)      => Left(error)
          case Right((a, rest)) => fn(a)(rest)
      )

    def >>=[B](fn: A => Parser[B]): Parser[B] = flatMap(fn)
    def >>[B](fb: => Parser[B]): Parser[B] = flatMap(_ => fb)

    def *>[B](that: Parser[B]): Parser[B] = (this ~ that).map(_._2)
    def <*[B](that: Parser[B]): Parser[A] = (this ~ that).map(_._1)
    def orElse[A1 >: A](that: Parser[A1]): Parser[A1] =
      Parser(
        inp =>
          parse(inp) match
            case Left(ParseError.Rip) => Left(ParseError.Rip)
            case Left(value)      => that(inp)
            case r @ Right(value) => r
      )
    def <|>[A1 >: A](that: Parser[A1]): Parser[A1] = orElse(that)

    def repeated: Parser[List[A]] = Parser(
      inp =>
        parse(inp) match
          case Left(value)      => Right((List.empty[A], inp))
          case Right((a, rest)) => repeated.map(a :: _)(rest)
    )
    def repeatedOne: Parser[List[A]] = 
      (this ~ this.repeated).map(_ :: _)

    def separatedOne[B](sep: Parser[B]): Parser[List[A]] =
      (this ~ ((sep *> this).repeated)).map((a, xs) => a :: xs)

    def separated[B](sep: Parser[B]): Parser[List[A]] =
      separatedOne(sep).orElse(pure(List.empty[A]))

    def withOptWhitespace = between(optWhitespace, this)

  }


  def elem: Parser[Elem] = Parser(
    inp =>
      if inp.isEmpty then Left(ParseError.UnexpectedEof)
      else Right(inp.first, inp.next),
  )


  def eof: Parser[Unit] = Parser(
    inp =>
      if inp.isEmpty then Right(((), inp))
      else Left(ParseError.ExpectedEof)
  )
  val Rip: Parser[Nothing] = Parser(_ => Left(ParseError.Rip))
  val Fail: Parser[Nothing] = Parser(_ => Left(ParseError.Fail))
  def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] =
    ff.flatMap(ab => fa.map(ab(_)))
  def <*>[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = ap(ff)(fa)
  def fail[A]: Parser[A] = Fail
  def rip[A]: Parser[A] = Rip

  def select[A, B](fa: Parser[Either[A, B]])(fn: Parser[A => B]): Parser[B] =
    fa.flatMap {
      case Left(a)          => fa *> fn.map(_(a))
      case r @ Right(value) => pure(value)
    }

  def map[A, B](fa: Parser[A])(fn: A => B) =
    Parser(inp => fa(inp).map((a, rest) => (fn(a), rest)))

  def lift[A, B](f: A => B): Parser[A] => Parser[B] =
    fa => fa.map(f)

  def lift2[A, B, C](f: A => B => C)(fa: Parser[A])(fb: Parser[B]): Parser[C] =
    ap(ap(pure(f))(fa))(fb)

  def combineK[A](fa: Parser[A], fb: Parser[A]): Parser[A] = fa.orElse(fb)

  def filter[A](fa: Parser[A])(fn: A => Boolean): Parser[A] = 
    fa.filter(fn)

  def mapFilter[A, B](fa: Parser[A])(fn: A => Option[B]): Parser[B] =
    fa.mapFilter(fn)

  def oneOf[A](ps: Iterable[Parser[A]]): Parser[A] =
    ps.foldLeft(fail)(_.orElse(_))


  def pure[A](a: A): Parser[A] = Parser(inp => Right(a, inp))

  def flatMap[A, B](fa: Parser[A])(fn: A => Parser[B]): Parser[B] =
    fa.flatMap(fn)

  def empty[A]: Parser[A] =
    Parser(inp => Left(ParseError.Fail))

  def sequence[A](ps: List[Parser[A]]): Parser[List[A]] = {
    def cons(a: A)(xs: List[A]) = (a :: xs)
    def consP(fa: Parser[A])(fxs: Parser[List[A]]) = lift2(cons)(fa)(fxs)
    val seqParser: Parser[List[A]] = ps match {
      case head :: tail => consP(head)(sequence(tail))
      case Nil          => pure(List.empty[A])
    }

    seqParser
  }

  def repeated[A](fa: Parser[A]): Parser[List[A]] = 
    fa.repeated

  def repeatedOne[A](fa: Parser[A]): Parser[List[A]] =
    fa.repeatedOne

  def single(e: Elem): Parser[Elem] =
    elem.filter(_ == e)

  def accept(e: Elem): Parser[Elem] =
    Parser{ inp =>
      if (inp.isEmpty) Left(ParseError.UnexpectedEof)  
      else if(inp.first == e) Right((e, inp.next))
      else Left(ParseError.UnexpectedChar(inp.first))
    }

  def anyOf(es: Set[Elem]): Parser[Elem] =
    oneOf(es.map(single))
  
  def anyOf(es: Seq[Elem]): Parser[Elem] =
    oneOf(es.map(single))  

  def anyExcept(es: Set[Elem]): Parser[Elem] =
    elem.filter(!es.contains(_))

  def anyExcept(e: Elem): Parser[Elem] =
    elem.filter(e != _)

  def anyExcept(es: Seq[Elem]): Parser[Elem] =
      elem.filter(!es.contains(_))

  def takeWhile(fn: Elem => Boolean): Parser[List[Elem]] = {
    repeated(elem.filter(fn))
  }

  def opt[A](fa: Parser[A]): Parser[Option[A]] =
    fa.map(Some(_)) <|> pure(None)

  def between[A, B, C](fa: Parser[A], fb: Parser[B], fc: Parser[C]): Parser[B] =
    fa *> fb <* fc

  def between[A, B](fa: Parser[A], fb: Parser[B]): Parser[B] =
    fa *> fb <* fa

  def separatedOne[A, B](fa: Parser[A], sep: Parser[B]): Parser[List[A]] =
    fa.separatedOne(sep)

  def separated[A, B](fa: Parser[A], sep: Parser[B]): Parser[List[A]] =
    fa.separated(sep)

  def whitespace: Parser[List[Char]] = Fail

  def optWhitespace: Parser[List[Char]] = pure(Nil)

  def withOptWhitespace[A](fa: Parser[A]) = fa

  implicit val catsInstanceParser: Monad[Parser] with Alternative[Parser] with FunctorFilter[Parser] = 
    new Monad[Parser] with Alternative[Parser] with FunctorFilter[Parser] {

      override def mapFilter[A, B](fa: Parser[A])(f: A => Option[B]): Parser[B] = fa.mapFilter(f)

      override def pure[A](x: A): Parser[A] = Parser(inp => Right((x, inp)))

      override def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] = 
        flatMap(f(a)) {
              case Right(b)    => pure(b)
              case Left(nextA) => tailRecM(nextA)(f)
            }

      override def functor: Functor[Parsers.this.Parser] = this

      override def empty[A]: Parser[A] = Parser(inp => Left(ParseError.Fail))

      override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = oneOf(x :: y:: Nil)

      override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = fa.flatMap(f)
    }

  // }
  object Parser{
    def apply(e:Elem) = accept(e)
    def apply[A](p: Input => ParseResult[A]) = new Parser(p)
    extension [A, B](p: Parser[A => B]) def <*>(fa: Parser[A]) = p.flatMap(ab => fa.map(ab(_)))
  }
}
