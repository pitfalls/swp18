import collection.mutable.Map

class Interpreter(reader: () => String, writer: String => _) {

  // Type aliases to make the signatures more readable
  type FunctionName = String
  type VariableName = String

  var stack = scala.collection.mutable.ListBuffer[collection.mutable.Map[String, ExpValue]]()

  val builtinFunctions: Map[FunctionName, List[ExpValue] => ExpValue] =
    Map(
      "eq?" -> { case List(a, b) => ExpBoolean(a == b) },

      //arithmetic
      "add" -> {case List(a:ExpInteger ,b:ExpInteger) => ExpInteger(a.i+b.i)},
      "sub" -> {case List(a:ExpInteger ,b:ExpInteger) => ExpInteger(a.i-b.i)},
      "lt?" -> {case List(a:ExpInteger, b:ExpInteger) => ExpBoolean(a.i < b.i)},

      //lists
      "build" -> { case List(a, b) => List(a, b) match {
        case List(a: ExpList, b: ExpList) => {
          ExpList(a.list ::: b.list)
        }
        case List(a: ExpInteger, b: ExpList) => {
          ExpList(a :: b.list)
        }
      }},
      "first" -> {case List(a: ExpList) => a.list.head},
      "rest" -> {case List(a:ExpList) => ExpList(a.list.tail)},

      //logic
        "and" -> {case List(a:ExpBoolean, b:ExpBoolean) => ExpBoolean(a.b && b.b)},
        "or" -> {case List(a:ExpBoolean, b:ExpBoolean) => ExpBoolean(a.b || b.b)},
        "not" -> {case List(a:ExpBoolean) => ExpBoolean(!a.b)},

      //IO
      "print" -> {case List(s: ExpString) => {
          writer(s.str)
          s
      }},
      "read" -> {
        case List() => ExpString(reader())
        case List(_) => ??? //print per def. requires an empty arg list
      }
    )

  var userFunctions = scala.collection.mutable.Map[String, Tuple2[List[String], Node]]()

  def interpret(program: Program): ExpValue = {
    // writer("Hello, " + reader() + "!")
    program.functions.map{case FunctionDeclaration(name, params, body) => addFunctionDeclaration(name,params,body)}
    evalNode(program.main)
  }

  def evalFuction(functionDef: Tuple2[List[String], Node], args: List[ExpValue]): ExpValue = {
    stack += collection.mutable.Map.empty //new stack frame
    (functionDef._1 zip args).map{case (id,value) => stack.last += (id -> value)}
    val result = evalNode(functionDef._2)
    stack.trimEnd(1)
    result
  }
  def evalBlock(blockBody: List[Node]) : ExpValue = {
    stack += collection.mutable.Map.empty //new stack frame
    val result = for (n <- blockBody) yield evalNode(n)
    stack.trimEnd(1)
    result.last
  }

  def evalNode(node: Node) : ExpValue = {
    node match {
      case Cond_Node(n1, n2, n3) => {
        if (evalNode(n1) match {
          case r: ExpBoolean => r.b
          case _ => ???
        }) {
          evalNode(n2)
        } else {
          evalNode(n3)
        }
      }
      case Call_Node(op:String, params: List[Node]) => {
        if(builtinFunctions.contains(op)) {
          builtinFunctions(op)(params.map(evalNode(_)))
        }else{
          evalFuction(userFunctions(op), params.map(evalNode(_)))
        }
      }
      case block_Node(body: List[Node]) => {
        evalBlock(body)
      }
      case VarDecl_Node(id: String, value: Node) => {
        val v = evalNode(value)
        stack.last += (id -> v)
        v
      }
      case VarAss_Node(id: String, value: Node) => {
        val v = evalNode(value)
        setVar(stack.reverseIterator,id,v)
        v
      }
      case Variable_Node(id) => {
        getVar(stack.reverseIterator,id)
      }
      case recordDef_Node(fields: List[recordValueDecl_Node]) => {
        var record = ExpRecord(collection.mutable.Map[String, ExpValue]())
        fields.map{
          case recordValueDecl_Node(id: String, value: Node)
          => record.data += (id -> evalNode(value))
        }
        record
      }
      case recordValueDecl_Node(v1: String, v2: Node) => {
        ???
      }
      case recordAccess_Node(r: Node, id: String) => {
        var record = evalNode(r)
        record match {
          case ExpRecord(data: collection.mutable.Map[String, ExpValue]) =>
            data(id)

          case _ => ???
        }
      }

      case List_Node(elem: List[Node]) => {
        ExpList(elem.map(evalNode(_)))
      }
      case Integer_Node(i: Int) => {
        ExpInteger(i)
      }
      case Bool_false_Node(value: String) => ExpBoolean(false)
      case Bool_True_Node(value: String) => ExpBoolean(true)
      case braces_Node(node: Node) => {
        evalNode(node)
      }
      case String_Node(s: String) => {
        ExpString(s)
      }

      case _ => ??? //FunctionDeclaration, Program, function_Node
    }
  }

  def addFunctionDeclaration(name: String, params: List[String], body: Node) =
  {
    userFunctions += (name -> Tuple2(params, body))
  }

  def getVar(frame: Iterator[scala.collection.mutable.Map[String,ExpValue]], id: String): ExpValue = {
    if(frame.hasNext){
      val v = frame.next().get(id)
      if(v.isDefined){
        v.get
      }else {
        getVar(frame,id)
      }
    }else{
        ??? //variable was never decleard! (undefined behavior)
    }
  }
  def setVar(frame: Iterator[scala.collection.mutable.Map[String,ExpValue]], id: String, value: ExpValue): ExpValue = {
    if(frame.hasNext){
      val current = frame.next()
      if(current.contains(id)){
        current(id) = value
        value
      }else {
        setVar(frame,id,value)
      }
    }else{
      ??? //no inital "stack frame" exists (will never happen)
    }
  }


  //DEBUG:
  def printStack() = {
    stack.foreach{f =>
      f.foreach(v => println(s"$v"))
      println("-----------------")
      f
    }
  }
}
