package itake

object Parser {
  def parse[S](tokens: Reader[Token, S], stream: S): Term = {
    exp(tokens).next(stream) match {
      case None => throw new Exception("no tokens")
      case Some(term -> stream) =>
        tokens.next(stream) match {
          case Some(Token.EOF -> stream) => term
          case Some(token -> stream) => throw new Exception(s"unexpected trailing token: $token")
          case None => throw new Exception(s"expected EOF")
        }
    }
  }

  def exp[S](tokens: Reader[Token, S]): Reader[Term, S] = {
    Reader { stream =>
      infexp(tokens).next(stream) match {
        case Some((exp, stream)) => Some((exp, stream))
        case None =>
          tokens.next(stream) match {
            case None => None
            case Some(Token.FN -> stream) => parseFn(tokens).next(stream)
            case Some(Token.IF -> stream) => parseIf(tokens).next(stream)
          }
      }
    }
  }

  def infexp[S](tokens: Reader[Token, S]): Reader[Term, S] = {
    def loop(a: Term, stream: S): Option[(Term, S)] = {
      tokens.next(stream) match {
        case Some((Token.ADD, stream)) =>
          app(tokens).next(stream) match {
            case Some((b, stream)) => loop(APP(APP(VAR("+"), a), b), stream)
            case None => throw new Exception("operator expected")
          }
        case Some((Token.SUB, stream)) =>
          app(tokens).next(stream) match {
            case Some((b, stream)) => loop(APP(APP(VAR("-"), a), b), stream)
            case None => throw new Exception("operator expected")
          }
        case _ => Some((a, stream))
      }
    }

    Reader { stream =>
      app(tokens).next(stream) match {
        case Some(a -> stream) => loop(a, stream)
        case None => None
      }
    }
  }

  def app[S](tokens: Reader[Token, S]): Reader[Term, S] = {
    def loop(a: Term, stream: S): Option[(Term, S)] = {
      atexp(tokens).next(stream) match {
        case None => Some(a -> stream)
        case Some(b -> stream) => loop(APP(a, b), stream)
      }
    }

    Reader { stream =>
      atexp(tokens).next(stream) match {
        case Some(a -> stream) => loop(a, stream)
        case None => None
      }
    }
  }

  def atexp[S](tokens: Reader[Token, S]): Reader[Term, S] = {
    Reader { stream =>
      tokens.next(stream) match {
        case Some(Token.LET -> stream)    => parseLet(tokens).next(stream)
        case Some(Token.LPAREN -> stream) => parseParens(tokens).next(stream)
        case Some(Token.INT(n) -> stream) => Some(INT(n) -> stream)
        case Some(Token.VAR(v) -> stream) => Some(VAR(v) -> stream)
        case Some(Token.TRUE -> stream)   => Some(BOOL(true) -> stream)
        case Some(Token.FALSE -> stream)  => Some(BOOL(false) -> stream)
        case _                            => None
      }
    }
  }

  def parseFn[S](tokens: Reader[Token, S]): Reader[Term, S] = {
    Reader { stream =>
      tokens.next(stream) match {
        case Some(Token.VAR(param) -> stream) =>
          tokens.next(stream) match {
            case Some(Token.DARROW -> stream) =>
              exp(tokens).next(stream) match {
                case Some(body -> stream) => Some(FUN(param, body) -> stream)
                case _ => throw new Exception("EXP expected")
              }
            case _ => throw new Exception("DARROW expected")
          }
        case _ => throw new Exception("VAR expected")
      }
    }
  }

  def parseIf[S](tokens: Reader[Token, S]): Reader[Term, S] = {
    Reader { stream =>
      exp(tokens).next(stream) match {
        case None => throw new Exception("EXP expected")
        case Some(predicate -> stream) =>
          tokens.next(stream) match {
            case Some(Token.THEN -> stream) =>
              exp(tokens).next(stream) match {
                case None => throw new Exception("EXP expected")
                case Some(thenBranch -> stream) =>
                  tokens.next(stream) match {
                    case Some(Token.ELSE -> stream) =>
                      exp(tokens).next(stream) match {
                        case None => throw new Exception("EXP expected")
                        case Some(elseBranch -> stream) =>
                          Some(IF(predicate, thenBranch, elseBranch) -> stream)
                      }
                    case _ => throw new Exception("ELSE expected")
                  }
              }
            case _ => throw new Exception("THEN expected")
          }
      }
    }
  }

  def parseParens[S](tokens: Reader[Token, S]): Reader[Term, S] = {
    Reader { stream =>
      exp(tokens).next(stream) match {
        case None => throw new Exception("VAL expected")
        case Some(a -> stream) =>
          tokens.next(stream) match {
            case Some(Token.RPAREN -> stream) => Some(a -> stream)
            case _ => throw new Exception("RPAREN expected")
          }
      }
    }
  }

  def parseLet[S](tokens: Reader[Token, S]): Reader[Term, S] = {
    Reader { stream =>
      tokens.next(stream) match {
        case Some(Token.VAL -> stream) =>
          tokens.next(stream) match {
            case Some(Token.VAR(name) -> stream) =>
              tokens.next(stream) match {
                case Some(Token.EQUAL -> stream) =>
                  exp(tokens).next(stream) match {
                    case None => throw new Exception("EXP expected")
                    case Some(value -> stream) =>
                      tokens.next(stream) match {
                        case Some(Token.IN -> stream) =>
                          exp(tokens).next(stream) match {
                            case None => throw new Exception("EXP expected")
                            case Some(body -> stream) =>
                              tokens.next(stream) match {
                                case Some(Token.END -> stream) =>
                                  Some(LET(name, value, body) -> stream)
                                case None => throw new Exception("END expected")
                              }
                          }
                        case _ => throw new Exception("IN expected")
                      }
                  }
                case _ => throw new Exception("EQUAL expected")
              }
            case _ => throw new Exception("VAR expected")
          }
        case _ => throw new Exception("VAL expected")
      }
    }
  }
}
