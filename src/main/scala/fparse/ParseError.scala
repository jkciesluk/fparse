package fparse

enum ParseError:
  case UnexpectedChar[Elem](e: Elem) extends ParseError
  case FilterError(msg: String = "") extends ParseError
  case ExpectedEof extends ParseError
  case UnexpectedEof extends ParseError
  case Fail extends ParseError
  case Failure(msg: String) extends ParseError
  case Rip extends ParseError

  override def toString(): String =
    this match
      case UnexpectedChar(e) => "Unexpected char: " + e
      case FilterError(msg) =>
        "Filter error" + (if msg.isEmpty() then msg else ": " + msg)
      case ExpectedEof   => "Expected end of file"
      case UnexpectedEof => "Unexpected end of file"
      case Fail          => "Parser failed"
      case Failure(msg) =>
        "Parser failure" + (if msg.isEmpty() then msg else ": " + msg)
      case Rip => "Rip: Report issue please"
