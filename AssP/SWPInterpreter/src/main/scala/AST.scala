sealed trait Node

case class Program(functions: List[FunctionDeclaration], main: Node) extends Node
case class FunctionDeclaration(name: String, params: List[String], body: Node) extends Node

// TODO Add case classes to represent the AST.
case class Integer_Node(value: Int) extends Node
case class Bool_false_Node(value: String) extends Node
case class Bool_True_Node(value: String) extends Node


case class Variable_Node(value: String) extends Node
case class List_Node(node:List[Node]) extends Node
case class Call_Node(value:String, node: List[Node]) extends Node
case class Cond_Node(n1: Node, n2: Node, n3: Node) extends Node
case class VarAss_Node(v1: String, v2: Node) extends Node
case class VarDecl_Node(v1: String, v2: Node) extends Node
case class braces_Node(v1: Node) extends Node
case class recordValueDecl_Node(v1: String, v2: Node) extends Node
case class recordDef_Node(v1: List[recordValueDecl_Node]) extends Node
case class block_Node(v1: List[Node]) extends Node
case class recordAccess_Node(v1: Node, v2: String) extends Node
case class function_Node(v1:Node, v2: List[Node], v3: Node) extends Node
