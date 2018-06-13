import scala.util.parsing.combinator._

class ExpParser extends JavaTokenParsers {

  // TODO Implement the expression parser and add additional parsers for terminal and non terminal symbols, where necessary.
  def program: Parser[Program] =
  rep(function<~";") ~ exp ^^ {case fs~ex => Program(fs, ex)}

  def exp: Parser[Node] = block | recordDef | recordAccess | varDecl | varAss |
                         cond | call | list | variable | int | Boolean_TRUE | Boolean_False | braces | stringliteral

  private def function : Parser[FunctionDeclaration] =
  "fun" ~ id ~ "(" ~ repsep(id, ",") ~ ")" ~ "=" ~ exp ^^ {case _~i~_~ids~_~_~ex => FunctionDeclaration(i, ids, ex)}

  private def braces: Parser[braces_Node] =
  "(" ~ exp ~ ")" ^^ {case _~x~_ => braces_Node(x)}


  private def block : Parser[block_Node] =
  "{" ~ rep(exp<~";") ~ "}" ^^ {case _~xs~_ => block_Node(xs)}

  private def varDecl: Parser[VarDecl_Node] =
    "$" ~ id ~ "=" ~ exp ^^{case _~id~_~x => VarDecl_Node(id, x)}

  private def varAss: Parser[VarAss_Node] =
    id ~ "=" ~ exp ^^{case id~_~x => VarAss_Node(id, x)}

    private def call: Parser[Call_Node] =
      id ~ "(" ~ repsep(exp, ",") ~ ")" ^^ {case i~_~xs~_ => Call_Node(i, xs)} //dont know if its correct

  private def cond: Parser[Cond_Node] =
  "if" ~ exp ~ "then" ~ exp ~ "else" ~ exp ^^ {case _~c1~_~c2~_~c3 => Cond_Node(c1, c2, c3)}

  private def list: Parser[List_Node] =
    "[" ~> repsep(exp, ",") <~ "]" ^^ {case xs => List_Node(xs)}

  private def variable: Parser[Variable_Node] =
    id ^^ {case id => Variable_Node(id)}

  private def recordvaluedecl : Parser[recordValueDecl_Node] =
  "$" ~ id ~ "=" ~ exp ^^ {case _~id~_~ex => recordValueDecl_Node(id, ex)}

  private def recordDef : Parser[recordDef_Node] =
  "object" ~ "{" ~ rep(recordvaluedecl<~";") ~ "}" ^^ {case _~_~rs~_ => recordDef_Node(rs)}

  def recordref : Parser[Node] = block | call | variable | recordDef | braces

  def recordAccess : Parser[recordAccess_Node] =
    recordref ~ "." ~ id ^^{case rf~_~i => recordAccess_Node(rf, i)}

  private def Boolean_False : Parser[Bool_True_Node] =
    boolean_true ^^ {case bool => Bool_True_Node(bool)}

  private def Boolean_TRUE : Parser[Bool_false_Node] =
    boolean_false ^^ {case bool => Bool_false_Node(bool)}

  def stringliteral: Parser[String_Node] =
    str ^^{case s => String_Node(s.substring(1, s.length-1))}

  //Terminale
  private val id : Parser[String] =
  "[a-z][A-Z|a-z|0-9|?|_]*".r

  private val int: Parser[Integer_Node] =
    """[0]|[1-9][0-9]*|-[1-9][0-9]*""".r ^^ {case a => Integer_Node(a.toInt)}

  private val boolean_false: Parser[String] =
    """FALSE"""

  private val boolean_true: Parser[String] =
    """TRUE"""

  private val str: Parser[String] =
    "\"([^\\|^|`]+?)\"".r

}

object ParseProgram extends ExpParser {
  def parse(s: String): ParseResult[Program] = {
    val s2 = s.replaceAll("(?s)#\\*.*\\*#","") //block comments
    val s3 = s2.replaceAll("(?m)#.*$","") //line comments

    parseAll(program, s3)
  }
}
