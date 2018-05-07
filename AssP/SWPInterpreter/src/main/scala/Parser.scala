import scala.util.parsing.combinator._

class ExpParser extends JavaTokenParsers {

  // TODO Implement the expression parser and add additional parsers for terminal and non terminal symbols, where necessary.
  def program: Parser[Program] = ???

  def exp: Parser[Expression_Node] = ???

  private def braces: Parser[braces_Node] =
  "(" ~ exp ~ ")" ^^ {case _~x~_ => braces_Node(x)}


  private def vardecl: Parser[VarDecl_Node] =
    "$" ~ Id ~ "=" ~ exp ^^{case _~id~_~x => VarDecl_Node(id, x)}

  private def varass: Parser[VarAss_Node] =
    Id ~ "=" ~ exp ^^{case id~_~x => VarAss_Node(id, x)}


   /* private def call: Parser[Call_Node] =
      Id ~ "(" ~> repsep(exp, ",") <~ ")" ^^ {case id~_~xs~_ => Call_Node(id, xs)}*/

  private def cond: Parser[Cond_Node] =
  "if" ~ exp ~ "then" ~ exp ~ "else" ~ exp ^^ {case _~c1~_~c2~_~c3 => Cond_Node(c1, c2, c3)}

  private def list: Parser[List_Node] =
    "[" ~> repsep(exp, ",") <~ "]" ^^ {case xs => List_Node(xs)}

  private def Variable: Parser[Variable_Node] =
    Id ^^ {case id => Variable_Node(id)}




  private def Id : Parser[ID_Node] =
    id ^^ {case identifier => ID_Node(identifier)}

  private def Boolean_False : Parser[Bool_True_Node] =
    boolean_true ^^ {case bool => Bool_True_Node(bool)}

  private def Boolean_TRUE : Parser[Bool_false_Node] =
    boolean_false ^^ {case bool => Bool_false_Node(bool)}

  //Terminale
  private val id : Parser[String] =
  "[a-z][A-Z|a-z|0-9|?|_]*".r

  private val int: Parser[Integer_Node] =
    """[0]|[1-9][0-9]*|-[1-9][0-9]*""".r ^^ {case a => Integer_Node(a.toInt)}

  private val boolean_false: Parser[String] =
    """FALSE"""

  private val boolean_true: Parser[String] =
    """TRUE"""







}

object ParseProgram extends ExpParser {
  def parse(s: String): ParseResult[Program] = {
    parseAll(program, s)
  }
}
