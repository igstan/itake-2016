package itake

trait Reader[E, S] {
  def next(stream: S): Option[(E, S)]

  def | (other: Reader[E, S]): Reader[E, S] = {
    Reader { stream =>
      next(stream) match {
        case None => other.next(stream)
        case some => some
      }
    }
  }
}

object Reader {
  def apply[E, S](fn: S => Option[(E, S)]): Reader[E, S] =
    new Reader[E, S] {
      def next(stream: S): Option[(E, S)] = fn(stream)
    }

  def success[E, S](e: E): Reader[E, S] =
    Reader(stream => Some(e -> stream))

  val string: Reader[Char, String] =
    new Reader[Char, String] {
      def next(stream: String): Option[(Char, String)] = {
        stream match {
          case "" => None
          case _ => Some(stream.head -> stream.tail)
        }
      }
    }

  def consume[E, S](reader: Reader[E, S], source: S): List[E] = {
    @annotation.tailrec
    def loop(source: S, result: List[E]): List[E] = {
      reader.next(source) match {
        case None | Some(Token.EOF -> _) => result.reverse
        case Some(t -> rest) => loop(rest, t :: result)
      }
    }

    loop(source, List.empty)
  }
}
