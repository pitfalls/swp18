package Ass1
import scala.util.parsing.combinator.JavaTokenParsers

object SimpleParserA extends JavaTokenParsers {
  def main(args: Array[String]): Unit = {
    val result = SimpleParserA("ac")
    println(result.get)
  }

  def A: Parser[Any] = "aa" ~ A | "c" ~ ""
  def B: Parser[Any] = "b" ~ B | "c" ~ ""
  def AB: Parser[Any] = A | B

  def S: Parser[Any] = "a" ~ AB | "ac"

  def apply(s: String) = parseAll(S, s)
}