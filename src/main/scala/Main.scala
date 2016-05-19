package itake

object Main {
  def main(args: Array[String]): Unit = {
    val tokens = Reader.consume(Lexer.lex(Reader.string), """
      let
        val inc = fn a => a + 1
      in
        inc 42
      end
    """)
    println(s"tokens: $tokens")

    val term = Parser.parse(Lexer.lex(Reader.string), """
      let
        val inc = fn a => a + 1
      in
        inc 42
      end
    """)
    println(s"term: $term")
  }
}
