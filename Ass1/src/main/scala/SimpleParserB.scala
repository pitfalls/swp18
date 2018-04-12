package Ass1
import scala.util.parsing.combinator.JavaTokenParsers

object SimpleParserB extends JavaTokenParsers {
  def main(args: Array[String]): Unit = {
    val result = SimpleParserB("2*10^6")
    println(result.get)
  }

  def N: Parser[Double] = "[1-9][0-9]*|0|-[1-9][0-9]*".r ^^ {i => i.toDouble}

  def S: Parser[Double] = N~"*10^"~N ^^ {case a~_~b => a*math.pow(10,b)}

  def apply(s: String) = parseAll(S, s)
}
