package fparse

import cats.implicits._
import cats.syntax.all._
import cats.Alternative
import cats.Applicative
import cats.Functor

import fparse.reader.InputReader
import cats.Monad
import cats.FunctorFilter
import cats.Foldable
import cats.kernel.Monoid
import fparse.positions.Position
trait Parsers[F[_]: Monad: Alternative: Foldable: FunctorFilter] {
  type Elem
  type Input = InputReader[Elem]

  enum ParseError:
    case UnexpectedChar(e: Elem) extends ParseError
    case FilterError(msg: String = "") extends ParseError
    case ExpectedEof extends ParseError
    case UnexpectedEof extends ParseError
    case Fail extends ParseError
    case Failure(msg: String) extends ParseError
    case Rip extends ParseError

  sealed trait ParseResult[+A] {
    def flatMapNext[B](fn: A => Input => ParseResult[B]): ParseResult[B]
    def flatMapNext[B](fn: (A, Input) => ParseResult[B]): ParseResult[B] =
      flatMapNext(a => inp => fn(a, inp))

    def filter(fn: A => Boolean, msg: String = ""): ParseResult[A]
    def mapFilter[B](fn: A => Option[B], msg: String = ""): ParseResult[B]

    def map[B](fn: A => B): ParseResult[B]
    def append[B >: A](fa: ParseResult[B]): ParseResult[B]
  }

  case class Success[A](res: F[(A, Input)]) extends ParseResult[A] {
    def lastFailure: Option[Failure] = None
    def lastPos: Input = res.maximumByOption(_._2)(InputReader.ord).get._2
    def flatMapNext[B](fn: A => Input => ParseResult[B]): ParseResult[B] =
      val res0: F[ParseResult[B]] = Monad[F].map(res) { case (a, rest) =>
        fn(a)(rest)
      }
      def iter(acc: ParseResult[B], pr: ParseResult[B]) =
        acc.append(pr)
      val nres =
        res0.reduceLeftOption(iter).getOrElse(Error("Impossible", None))

      nres

    def map[B](fn: A => B): ParseResult[B] =
      Success(
        Monad[F].map(res) { case (a, rest) => (fn(a), rest) },
        lastFailure
      )

    def append[B >: A](fa: ParseResult[B]): ParseResult[A] =
      fa match
        case s @ Success(res0) =>
          Success(
            res.combineK(res0.asInstanceOf[F[(A, Input)]]),
            pickLastFailure(this.lastFailure, s.lastFailure)
          )
        case Error(msg, _)     => this
        case Failure(err, inp) => this

    def filter(fn: A => Boolean, msg: String = ""): ParseResult[A] =
      val res0 = res.filter((a, _) => fn(a))
      if res0.isEmpty then
        val inp = lastPos
        Failure(ParseError.FilterError(msg), inp)
      else Success(res0, lastFailure)

    def mapFilter[B](fn: A => Option[B], msg: String = ""): ParseResult[B] =
      val res0 = res.mapFilter((a, rest) => fn(a).map(b => (b, rest)))
      if res0.isEmpty then
        val inp = lastPos
        Failure(ParseError.FilterError(msg), inp)
      else Success(res0)

  }

  private def Success[A](res: F[(A, Input)], failure: Option[Failure]) =
    new Success(res) {
      override def lastFailure: Option[Failure] = failure
    }

  private def pickLastFailure(s1: Option[Failure], s2: Option[Failure]) = {
    (s1, s2) match
      case (Some(f1), Some(f2)) =>
        if f1.pos.compare(f2.pos) > 0 then Some(f1)
        else Some(f2)
      case (Some(f1), _) => Some(f1)
      case (_, Some(f2)) => Some(f2)
      case _             => None

  }

  object Success {
    def pure[A](a: A, inp: Input) = Success(Monad[F].pure((a, inp)))
    def emptyList[A](inp: Input, lastFailure: Option[Failure] = None) =
      Success(Monad[F].pure((List.empty[A], inp)), lastFailure)
  }

  sealed abstract class NoSuccess(err: ParseError)
      extends ParseResult[Nothing] {
    def flatMapNext[B](fn: Nothing => Input => ParseResult[B]): ParseResult[B] =
      this

    def map[B](fn: Nothing => B): ParseResult[B] =
      this

    def append[A >: Nothing](fa: ParseResult[A]): ParseResult[A]

    def filter(fn: Nothing => Boolean, msg: String = ""): ParseResult[Nothing] =
      this
    def mapFilter[B](
        fn: Nothing => Option[B],
        msg: String = ""
    ): ParseResult[B] = this
  }

  case class Error(msg: String, inp: Option[Input])
      extends NoSuccess(ParseError.Rip) {
    def append[A >: Nothing](fa: ParseResult[A]): ParseResult[A] = this
  }

  case class Failure(err: ParseError, inp: Input) extends NoSuccess(err) {
    def append[A >: Nothing](fa: ParseResult[A]): ParseResult[A] = fa match
      case s @ Success(res) =>
        Success(res, pickLastFailure(s.lastFailure, Some(this)))
      case f: Failure => pickLastFailure(Some(this), Some(f)).get
      case e: Error   => e

    def pos = inp.pos
  }

  type Label = String

  class Parser[+A](parse0: Input => ParseResult[A]) {

    def parse(inp: Input): ParseResult[A] = parse0(inp)
    def apply(inp: Input) = parse(inp)

    def ? : Parser[Option[A]] =
      this.map(Some(_)) <|> pure(None)
    def map[B](fn: A => B): Parser[B] =
      Parser(inp => parse(inp).map(fn))
    def ^^[B](fn: A => B): Parser[B] = map(fn)

    def filter(fn: A => Boolean) = Parser { inp =>
      parse(inp).filter(fn)
    }

    def mapFilter[B](fn: A => Option[B]): Parser[B] = Parser { inp =>
      parse(inp).mapFilter(fn)
    }

    def void: Parser[Unit] =
      Parser(inp => parse(inp).map(_ => ()))

    def ~[B](that: Parser[B]): Parser[(A, B)] =
      Parser(inp =>
        parse(inp).flatMapNext((a, rest) => that(rest).map(b => (a, b)))
      )

    def andThen[B](that: Parser[B]): Parser[(A, B)] = this ~ that

    def flatMap[B](fn: A => Parser[B]): Parser[B] =
      Parser(inp => parse(inp).flatMapNext((a, rest) => fn(a)(rest)))

    def >>=[B](fn: A => Parser[B]): Parser[B] = flatMap(fn)
    def >>[B](fb: => Parser[B]): Parser[B] = flatMap(_ => fb)

    def *>[B](that: Parser[B]): Parser[B] = (this ~ that).map(_._2)
    def <*[B](that: Parser[B]): Parser[A] = (this ~ that).map(_._1)
    def orElse[A1 >: A](that: Parser[A1]): Parser[A1] =
      Parser(inp => parse(inp).append(that(inp)))
    def <|>[A1 >: A](that: Parser[A1]): Parser[A1] = orElse(that)

    def repeated: Parser[List[A]] = Parser(inp =>
      parse(inp) match
        case e: Error => e
        case f @ Failure(err, inp) =>
          Success.emptyList(inp, Some(f))
        case s @ Success(res) =>
          val nextRes = s.flatMapNext((a, rest) => repeated.map(a :: _)(rest))
          // val app = s.map(a => List(a))
          nextRes.append(Success.emptyList(inp))
    )

    def repeatedOne: Parser[List[A]] =
      (this ~ this.repeated).map(_ :: _)

    def separatedOne[B](sep: Parser[B]): Parser[List[A]] =
      (this ~ ((sep *> this).repeated)).map((a, xs) => a :: xs)

    def separated[B](sep: Parser[B]): Parser[List[A]] =
      separatedOne(sep).orElse(pure(List.empty[A]))

    def repeatedLzy: Parser[List[A]] = Parser(inp =>
      parse(inp) match
        case e: Error => e
        case f @ Failure(err, inp) =>
          Success.emptyList(inp, Some(f))
        case s @ Success(res) =>
          val nextRes =
            s.flatMapNext((a, rest) => repeatedLzy.map(a :: _)(rest))
          // val app = s.map(a => List(a))
          Success.emptyList(inp).append(nextRes)
    )

    def repeatedOneLzy: Parser[List[A]] =
      (this ~ this.repeatedLzy).map(_ :: _)

    def separatedOneLzy[B](sep: Parser[B]): Parser[List[A]] =
      (this ~ ((sep *> this).repeatedLzy)).map((a, xs) => a :: xs)

    def separatedLzy[B](sep: Parser[B]): Parser[List[A]] =
      separatedOneLzy(sep).orElse(pure(List.empty[A]))

    def repeatedN(n: Int): Parser[List[A]] =
      if n <= 0 then pure(List.empty[A])
      else (this ~ repeatedN(n - 1)).map(_ :: _)

    def repeatedMaxN(n: Int): Parser[List[A]] = Parser(inp =>
      parse(inp) match
        case e: Error => e
        case f @ Failure(err, inp) =>
          Success.emptyList(inp, Some(f))

        case s @ Success(res) =>
          if (n <= 0) Success.emptyList(inp)
          else
            val nextRes =
              s.flatMapNext((a, rest) => repeatedMaxN(n - 1).map(a :: _)(rest))
            // val app = s.map(a => List(a))
            nextRes.append(Success.emptyList(inp))
    )

    def repeatedMinN(n: Int, m: Option[Int] = None): Parser[List[A]] = {
      val parser0 = repeatedN(n)
      val parser1 = m match
        case None        => repeated
        case Some(value) => repeatedMaxN(value)
      (parser0 ~ parser1).map(_ ++ _)
    }

    def repeatedMaxNLzy(n: Int): Parser[List[A]] = Parser(inp =>
      parse(inp) match
        case e: Error => e
        case f @ Failure(err, inp) =>
          Success.emptyList(inp, Some(f))
        case s @ Success(res) =>
          if (n <= 0) Success.emptyList(inp)
          else
            val nextRes =
              s.flatMapNext((a, rest) => repeatedMaxN(n - 1).map(a :: _)(rest))
            // val app = s.map(a => List(a))
            Success.emptyList(inp).append(nextRes)
    )

    def repeatedMinNLzy(n: Int, m: Option[Int] = None): Parser[List[A]] = {
      val parser0 = repeatedN(n)
      val parser1 = m match
        case None        => repeated
        case Some(value) => repeatedMaxNLzy(value)
      (parser0 ~ parser1).map(_ ++ _)
    }

    def withOptWhitespace = between(optWhitespace, this)
    def debug(msg: String = "") = Parser(inp =>
      pprint.log(inp)
      if msg.nonEmpty then pprint.log(msg)
      parse(inp)
    )

    def run(inp: Input): A = (this <* eof).parse(inp) match
      case Success(res)      => res.find(_ => true).get._1
      case Error(msg, _)     => throw Exception(msg)
      case Failure(err, inp) => throw Exception(err.toString())

  }

  def elem: Parser[Elem] = Parser(inp =>
    if inp.isEmpty then Failure(ParseError.UnexpectedEof, inp)
    else Success(Monad[F].pure((inp.first, inp.next))),
  )

  def eof: Parser[Unit] = Parser(inp =>
    if inp.isEmpty then Success(Monad[F].pure(((), inp)))
    else Failure(ParseError.ExpectedEof, inp)
  )
  val Rip: Parser[Nothing] = Parser(inp => Error("Rip", Some(inp)))
  val Fail: Parser[Nothing] = Parser(inp => Failure(ParseError.Fail, inp))
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
    fa.map(fn)

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

  def pure[A](a: A): Parser[A] = Parser(inp => Success.pure(a, inp))

  def flatMap[A, B](fa: Parser[A])(fn: A => Parser[B]): Parser[B] =
    fa.flatMap(fn)

  def empty[A]: Parser[A] =
    Parser(inp => Failure(ParseError.Fail, inp))

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

  def repeatedLzy[A](fa: Parser[A]): Parser[List[A]] =
    fa.repeatedLzy

  def repeatedOneLzy[A](fa: Parser[A]): Parser[List[A]] =
    fa.repeatedOneLzy

  def repeatedN[A](fa: Parser[A])(n: Int): Parser[List[A]] =
    fa.repeatedN(n)

  def repeatedMaxN[A](fa: Parser[A])(n: Int): Parser[List[A]] =
    fa.repeatedMaxN(n)

  def repeatedMinN[A](
      fa: Parser[A]
  )(n: Int, m: Option[Int] = None): Parser[List[A]] =
    fa.repeatedMinN(n, m)

  def repeatedMaxNLzy[A](fa: Parser[A])(n: Int): Parser[List[A]] =
    fa.repeatedMaxNLzy(n)

  def repeatedMinNLzy[A](
      fa: Parser[A]
  )(n: Int, m: Option[Int] = None): Parser[List[A]] =
    fa.repeatedMinNLzy(n, m)

  def accept(e: Elem): Parser[Elem] =
    Parser { inp =>
      if (inp.isEmpty) Failure(ParseError.UnexpectedEof, inp)
      else if (inp.first == e) Success.pure(inp.first, inp.next)
      else Failure(ParseError.UnexpectedChar(inp.first), inp)
    }

  def accept(fn: Elem => Boolean): Parser[Elem] =
    Parser { inp =>
      if (inp.isEmpty) Failure(ParseError.UnexpectedEof, inp)
      else if (fn(inp.first)) Success.pure(inp.first, inp.next)
      else Failure(ParseError.UnexpectedChar(inp.first), inp)
    }

  def anyOf(es: Set[Elem]): Parser[Elem] =
    oneOf(es.map(accept))

  def anyOf(es: Seq[Elem]): Parser[Elem] =
    oneOf(es.map(accept))

  def anyExcept(es: Set[Elem]): Parser[Elem] =
    accept(!es.contains(_))

  def anyExcept(e: Elem): Parser[Elem] =
    accept(e != _)

  def anyExcept(es: Seq[Elem]): Parser[Elem] =
    accept(!es.contains(_))

  def takeWhile(fn: Elem => Boolean): Parser[List[Elem]] = {
    repeated(accept(fn))
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

  def separatedOneLzy[A, B](fa: Parser[A], sep: Parser[B]): Parser[List[A]] =
    fa.separatedOneLzy(sep)

  def separatedLzy[A, B](fa: Parser[A], sep: Parser[B]): Parser[List[A]] =
    fa.separatedLzy(sep)

  def whitespace: Parser[List[Char]] = Fail

  def optWhitespace: Parser[List[Char]] = pure(Nil)

  def withOptWhitespace[A](fa: Parser[A]) = fa

  implicit val catsInstanceParser
      : Monad[Parser] with Alternative[Parser] with FunctorFilter[Parser] =
    new Monad[Parser] with Alternative[Parser] with FunctorFilter[Parser] {

      override def mapFilter[A, B](fa: Parser[A])(
          f: A => Option[B]
      ): Parser[B] = fa.mapFilter(f)

      override def pure[A](x: A): Parser[A] =
        Parser(inp => Success.pure(x, inp))

      override def tailRecM[A, B](
          a: A
      )(f: A => Parser[Either[A, B]]): Parser[B] =
        flatMap(f(a)) {
          case Right(b)    => pure(b)
          case Left(nextA) => tailRecM(nextA)(f)
        }

      override def functor: Functor[Parsers.this.Parser] = this

      override def empty[A]: Parser[A] =
        Parser(inp => Failure(ParseError.Fail, inp))

      override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = oneOf(
        x :: y :: Nil
      )

      override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] =
        fa.flatMap(f)
    }

  // }
  object Parser {
    def apply(e: Elem) = accept(e)
    def apply[A](p: Input => ParseResult[A]) = new Parser(p)
    extension [A, B](p: Parser[A => B])
      def <*>(fa: Parser[A]) = p.flatMap(ab => fa.map(ab(_)))
  }
}
