
trait Result[+A]
case class Success[+A](get:A, charsConsumed: Int) extends Result[A]
case class Failure(get:ParseError) extends Result[Nothing]
object Parser {
  type Parser[+A] = String => Either[ParseError, A]
  type sParser[+A] = Location=>Result[A]
}