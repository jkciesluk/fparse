package fparse

enum ParseError:
    case UnexpectedChar[Elem](e: Elem) extends ParseError
    case FilterError(msg: String = "") extends ParseError
    case ExpectedEof extends ParseError
    case UnexpectedEof extends ParseError
    case Fail extends ParseError
    case Failure(msg: String) extends ParseError
    case Rip extends ParseError