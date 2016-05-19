package itake
package test

class ParserTest extends FunSuite with Matchers {
  test("parses addition") {
    val lexer = Lexer.lex(Reader.string)
    Parser.parse(lexer, "1 + 2") should be {
      APP(APP(VAR("+"), INT(1)), INT(2))
    }
  }

  test("parses subtraction") {
    val lexer = Lexer.lex(Reader.string)
    Parser.parse(lexer, "a + b") should be {
      APP(APP(VAR("+"), VAR("a")), VAR("b"))
    }
  }

  test("parses right-parenthesized expressions") {
    val lexer = Lexer.lex(Reader.string)
    Parser.parse(lexer, "a + (b + 2)") should be {
      APP(
        APP(VAR("+"), VAR("a")),
        APP(APP(VAR("+"), VAR("b")), INT(2))
      )
    }
  }

  test("parses left-parenthesized expressions") {
    val lexer = Lexer.lex(Reader.string)
    Parser.parse(lexer, "(a + b) + 2") should be {
      APP(
        APP(
          VAR("+"),
          APP(
            APP(VAR("+"), VAR("a")),
            VAR("b")
          )
        ),
        INT(2)
      )
    }
  }

  test("parses let expressions") {
    val lexer = Lexer.lex(Reader.string)
    Parser.parse(lexer, "let val a = 1 in a end") should be {
      LET("a", INT(1), VAR("a"))
    }
  }

  test("parses fn expressions") {
    val lexer = Lexer.lex(Reader.string)
    Parser.parse(lexer, "fn a => a") should be {
      FUN("a", VAR("a"))
    }
  }

  test("parses if expressions") {
    val lexer = Lexer.lex(Reader.string)
    Parser.parse(lexer, "if true then 1 else 2") should be {
      IF(BOOL(true), INT(1), INT(2))
    }
  }
}
