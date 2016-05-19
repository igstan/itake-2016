package itake
package test

class LexerTest extends FunSuite with Matchers {
  test("lexes =>") {
    Lexer.symbol(Reader.string).next("=>") should be {
      Some(Token.DARROW -> "")
    }
  }

  test("lexes =") {
    Lexer.symbol(Reader.string).next("=") should be {
      Some(Token.EQUAL -> "")
    }
  }

  test("lexes +") {
    Lexer.symbol(Reader.string).next("+") should be {
      Some(Token.ADD -> "")
    }
  }

  test("lexes -") {
    Lexer.symbol(Reader.string).next("-") should be {
      Some(Token.SUB -> "")
    }
  }

  test("lexes (") {
    Lexer.symbol(Reader.string).next("(") should be {
      Some(Token.LPAREN -> "")
    }
  }

  test("lexes )") {
    Lexer.symbol(Reader.string).next(")") should be {
      Some(Token.RPAREN -> "")
    }
  }

  test("lexes 0") {
    Lexer.integer(Reader.string).next("00") should be {
      Some(Token.INT(0) -> "0")
    }
  }

  test("lexes 10") {
    Lexer.integer(Reader.string).next("10") should be {
      Some(Token.INT(10) -> "")
    }
  }

  test("lexes 123") {
    Lexer.integer(Reader.string).next("123") should be {
      Some(Token.INT(123) -> "")
    }
  }

  test("lexes identifier foo") {
    Lexer.identifier(Reader.string).next("foo") should be {
      Some(Token.VAR("foo") -> "")
    }
  }

  test("lexes keywords") {
    Lexer.identifier(Reader.string).next("if") should be(Some(Token.IF -> ""))
    Lexer.identifier(Reader.string).next("then") should be(Some(Token.THEN -> ""))
    Lexer.identifier(Reader.string).next("else") should be(Some(Token.ELSE -> ""))
    Lexer.identifier(Reader.string).next("fn") should be(Some(Token.FN -> ""))
    Lexer.identifier(Reader.string).next("let") should be(Some(Token.LET -> ""))
    Lexer.identifier(Reader.string).next("val") should be(Some(Token.VAL -> ""))
    Lexer.identifier(Reader.string).next("in") should be(Some(Token.IN -> ""))
    Lexer.identifier(Reader.string).next("end") should be(Some(Token.END -> ""))
    Lexer.identifier(Reader.string).next("true") should be(Some(Token.TRUE -> ""))
    Lexer.identifier(Reader.string).next("false") should be(Some(Token.FALSE -> ""))
  }

  test("lexes => with lex") {
    Lexer.lex(Reader.string).next("=>") should be {
      Some(Token.DARROW -> "")
    }
  }

  test("lexes = with lex") {
    Lexer.lex(Reader.string).next("=") should be {
      Some(Token.EQUAL -> "")
    }
  }

  test("lexes + with lex") {
    Lexer.lex(Reader.string).next("+") should be {
      Some(Token.ADD -> "")
    }
  }

  test("lexes - with lex") {
    Lexer.lex(Reader.string).next("-") should be {
      Some(Token.SUB -> "")
    }
  }

  test("lexes ( with lex") {
    Lexer.lex(Reader.string).next("(") should be {
      Some(Token.LPAREN -> "")
    }
  }

  test("lexes ) with lex") {
    Lexer.lex(Reader.string).next(")") should be {
      Some(Token.RPAREN -> "")
    }
  }

  test("lexes 0 with lex") {
    Lexer.lex(Reader.string).next("00") should be {
      Some(Token.INT(0) -> "0")
    }
  }

  test("lexes 10 with lex") {
    Lexer.lex(Reader.string).next("10") should be {
      Some(Token.INT(10) -> "")
    }
  }

  test("lexes 123 with lex") {
    Lexer.lex(Reader.string).next("123") should be {
      Some(Token.INT(123) -> "")
    }
  }

  test("lexes identifier foo with lex") {
    Lexer.lex(Reader.string).next("foo") should be {
      Some(Token.VAR("foo") -> "")
    }
  }

  test("lexes keywords with lex") {
    Lexer.lex(Reader.string).next("if") should be(Some(Token.IF -> ""))
    Lexer.lex(Reader.string).next("then") should be(Some(Token.THEN -> ""))
    Lexer.lex(Reader.string).next("else") should be(Some(Token.ELSE -> ""))
    Lexer.lex(Reader.string).next("fn") should be(Some(Token.FN -> ""))
    Lexer.lex(Reader.string).next("let") should be(Some(Token.LET -> ""))
    Lexer.lex(Reader.string).next("val") should be(Some(Token.VAL -> ""))
    Lexer.lex(Reader.string).next("in") should be(Some(Token.IN -> ""))
    Lexer.lex(Reader.string).next("end") should be(Some(Token.END -> ""))
    Lexer.lex(Reader.string).next("true") should be(Some(Token.TRUE -> ""))
    Lexer.lex(Reader.string).next("false") should be(Some(Token.FALSE -> ""))
  }

  test("reports EOF") {
    Lexer.lex(Reader.string).next("") should be(Some(Token.EOF -> ""))
  }
}
