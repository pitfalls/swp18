sealed trait Node

case class Program(functions: List[FunctionDeclaration], main: Node) extends Node
case class FunctionDeclaration(name: String, params: List[String], body: Node) extends Node
case class Expression_Node(node: Node) extends Node

// TODO Add case classes to represent the AST.
case class Integer_Node(value: Int) extends Node
case class Bool_false_Node(value: String) extends Node
case class Bool_True_Node(value: String) extends Node
case class ID_Node(value:String) extends Node


case class Variable_Node(value: Node) extends Node
case class List_Node(node:List[Expression_Node]) extends Node
case class Call_Node(value:Node, node: List[Expression_Node]) extends Node
case class Cond_Node(n1: Node, n2: Node, n3: Node) extends Node
case class VarAss_Node(v1: Node, v2: Expression_Node) extends Node
case class VarDecl_Node(v1: Node, v2: Expression_Node) extends Node
case class braces_Node(v1: Expression_Node) extends Node

