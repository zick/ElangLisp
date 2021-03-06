def kLPar := '('
def kRPar := ')'
def kQuote := '\''

def kNil {
  to tag() { return "nil" }
}

def makeNum(n) {
  def num {
    to tag() { return "num" }
    to data() { return n }
  }
  return num
}

def symTable := ["nil" => kNil].diverge()
def makeSym(s) {
  if (!symTable.maps(s)) {
    def sym {
      to tag() { return "sym" }
      to data() { return s }
    }
    symTable[s] := sym
  }
  return symTable[s]
}
def sym_t := makeSym("t")
def sym_quote := makeSym("quote")
def sym_if := makeSym("if")
def sym_lambda := makeSym("lambda")
def sym_defun := makeSym("defun")
def sym_setq := makeSym("setq")

def makeError(s) {
  def err {
    to tag() { return "error" }
    to data() { return s }
  }
  return err
}

def makeCons(var a, var d) {
  def cons {
    to tag() { return "cons" }
    to car() { return a }
    to cdr() { return d }
    to setCar(v) { a := v }
    to setCdr(v) { d := v }
  }
  return cons
}

def makeSubr(f) {
  def subr {
    to tag() { return "subr" }
    to call(args) { return f(args) }
  }
  return subr
}

def safeCar(obj) {
  if (obj.tag() == "cons") {
    return obj.car()
  }
  return kNil
}

def safeCdr(obj) {
  if (obj.tag() == "cons") {
    return obj.cdr()
  }
  return kNil
}

def makeExpr(args, env) {
  def expr {
    to tag() { return "expr" }
    to args() { return safeCar(args) }
    to body() { return safeCdr(args) }
    to env() { return env }
  }
  return expr
}

def nreverse(var lst) {
  var ret := kNil
  while (lst.tag() == "cons") {
    def tmp := lst.cdr()
    lst.setCdr(ret)
    ret := lst
    lst := tmp
  }
  return ret
}

def pairlis(var lst1, var lst2) {
  var ret := kNil
  while (lst1.tag() == "cons" && lst2.tag() == "cons") {
    ret := makeCons(makeCons(lst1.car(), lst2.car()), ret)
    lst1 := lst1.cdr()
    lst2 := lst2.cdr()
  }
  return nreverse(ret)
}

def isSpace(c) {
  return c == '\t' || c == '\r' || c == '\n' || c == ' '
}

def isDelimiter(c) {
  return c == kLPar || c == kRPar || c == kQuote || isSpace(c)
}

def skipSpaces(str) {
  var i := 0
  while (i < str.size()) {
    if (!isSpace(str[i])) {
      break
    }
    i += 1
  }
  return str(i, str.size())
}

def numMap :=
    ['0'=>0,'1'=>1,'2'=>2,'3'=>3,'4'=>4,'5'=>5,'6'=>6,'7'=>7,'8'=>8,'9'=>9]
def toNum(c) {
  return numMap[c]
}
def isNumChar(c) {
  return numMap.maps(c)
}

def makeNumOrSym(str) {
  var i := 0
  var sign := 1
  if (str[0] == '-') {
    sign := -1
    i := 1
  }
  var is_num := false
  var num := 0
  while (i < str.size()) {
    if (isNumChar(str[i])) {
      is_num := true
      num := num * 10 + toNum(str[i])
    } else {
      is_num := false
      break
    }
    i += 1
  }
  if (is_num) {
    return makeNum(num * sign)
  } else {
    return makeSym(str)
  }
}

def readAtom(var str) {
  var next := ""
  var i := 0
  while (i < str.size()) {
    if (isDelimiter(str[i])) {
      next := str(i, str.size())
      str := str(0, i)
      break
    }
    i += 1
  }
  return [makeNumOrSym(str), next]
}

var readList := null

def read(var str) {
  str := skipSpaces(str)
  if (str.size() == 0) {
    return [makeError("empty input"), ""]
  } else if (str[0] == kRPar) {
    return [makeError("invalid syntax: " + str), ""]
  } else if (str[0] == kLPar) {
    return readList(str(1, str.size()))
  } else if (str[0] == kQuote) {
    def [elm, next] := read(str(1, str.size()))
    return [makeCons(sym_quote, makeCons(elm, kNil)), next]
  } else {
    return readAtom(str)
  }
}

def readListImpl(var str) {
  var ret := kNil
  while (true) {
    str := skipSpaces(str)
    if (str.size() == 0) {
      return [makeError("unfinished parenthesis"), ""]
    } else if (str[0] == kRPar) {
      break
    }
    def [elm, next] := read(str)
    if (elm.tag() == "error") {
      return [elm, next]
    }
    ret := makeCons(elm, ret)
    str := next
  }
  return [nreverse(ret), str(1, str.size())]
}
readList := readListImpl

var printList := null

def printObj(obj) {
  if (obj == kNil) {
    return "nil";
  } else if (obj.tag() == "num") {
    return `${obj.data()}`
  } else if (obj.tag() == "sym") {
    return obj.data()
  } else if (obj.tag() == "error") {
    return "<error: " + obj.data() + ">"
  } else if (obj.tag() == "cons") {
    return printList(obj)
  } else if (obj.tag() == "subr") {
    return "<subr>"
  } else if (obj.tag() == "expr") {
    return "<expr>"
  } else {
    return "<unknown>"
  }
}

def printListImpl(var obj) {
  var ret := ""
  var first := true
  while (obj.tag() == "cons") {
    if (first) {
      first := false
    } else {
      ret += " "
    }
    ret += printObj(obj.car())
    obj := obj.cdr()
  }
  if (obj == kNil) {
    return "(" + ret + ")"
  } else {
    return "(" + ret + " . " + printObj(obj) + ")"
  }
}
printList := printListImpl

def findVar(sym, var env) {
  while (env.tag() == "cons") {
    var alist := env.car()
    while (alist.tag() == "cons") {
      if (alist.car().car() == sym) {
        return alist.car()
      }
      alist := alist.cdr()
    }
    env := env.cdr()
  }
  return kNil
}

def g_env := makeCons(kNil, kNil)

def addToEnv(sym, val, env) {
  env.setCar(makeCons(makeCons(sym, val), env.car()))
}

var evlis := null
var apply := null

def eval(obj, env) {
  if (obj == kNil || obj.tag() == "num" || obj.tag() == "error") {
    return obj
  } else if (obj.tag() == "sym") {
    def bnd := findVar(obj, env)
    if (bnd == kNil) {
      return makeError(obj.data() + " has no value")
    }
    return bnd.cdr()
  }

  def op := safeCar(obj)
  def args := safeCdr(obj)
  if (op == sym_quote) {
    return safeCar(args)
  } else if (op == sym_if) {
    def c := eval(safeCar(args), env)
    if (c.tag() == "error") {
      return c
    } else if (c == kNil) {
      return eval(safeCar(safeCdr(safeCdr(args))), env)
    }
    return eval(safeCar(safeCdr(args)), env)
  } else if (op == sym_lambda) {
    return makeExpr(args, env)
  } else if (op == sym_defun) {
    def expr := makeExpr(safeCdr(args), env)
    def sym := safeCar(args)
    addToEnv(sym, expr, g_env)
    return sym
  } else if (op == sym_setq) {
    def val := eval(safeCar(safeCdr(args)), env)
    def sym := safeCar(args)
    def bnd := findVar(sym, env)
    if (bnd == kNil) {
      addToEnv(sym, val, g_env)
    } else {
      bnd.setCdr(val)
    }
    return val
  }
  return apply(eval(op, env), evlis(args, env))
}

def evlisImpl(var lst, env) {
  var ret := kNil
  while (lst.tag() == "cons") {
    def elm := eval(lst.car(), env)
    if (elm.tag() == "error") {
      return elm
    }
    ret := makeCons(elm, ret)
    lst := lst.cdr()
  }
  return nreverse(ret)
}
evlis := evlisImpl

def progn(var body, env) {
  var ret := kNil
  while (body.tag() == "cons") {
    ret := eval(body.car(), env)
    body := body.cdr()
  }
  return ret
}

def applyImpl(func, args) {
  if (func.tag() == "error") {
    return func
  } else if (args.tag() == "error") {
    return args
  } else if (func.tag() == "subr") {
    return func.call(args)
  } else if (func.tag() == "expr") {
    return progn(func.body(), makeCons(pairlis(func.args(), args), func.env()))
  }
  return makeError(printObj(func) + " is not function")
}
apply := applyImpl

def subrCar(args) {
  return safeCar(safeCar(args))
}

def subrCdr(args) {
  return safeCdr(safeCar(args))
}

def subrCons(args) {
  return makeCons(safeCar(args), safeCar(safeCdr(args)))
}

def subrEq(args) {
  def x := safeCar(args)
  def y := safeCar(safeCdr(args))
  if (x.tag() == "num" && y.tag() == "num") {
    if (x.data() == y.data()) {
      return sym_t
    }
    return kNil
  } else if (x == y) {
    return sym_t
  }
  return kNil
}

def subrAtom(args) {
  if (safeCar(args).tag() == "cons") {
    return kNil
  }
  return sym_t
}

def subrNumberp(args) {
  if (safeCar(args).tag() == "num") {
    return sym_t
  }
  return kNil
}

def subrSymbolp(args) {
  if (safeCar(args).tag() == "sym") {
    return sym_t
  }
  return kNil
}

def subrAddOrMul(calc, init_val) {
  def subr(var args) {
    var ret := init_val
    while (args.tag() == "cons") {
      if (args.car().tag() != "num") {
        return makeError("wrong type")
      }
      ret := calc(ret, args.car().data())
      args := args.cdr()
    }
    return makeNum(ret)
  }
  return subr
}
def calcAdd(x, y) { return x + y }
def subrAdd := subrAddOrMul(calcAdd, 0)
def calcMul(x, y) { return x * y }
def subrMul := subrAddOrMul(calcMul, 1)

def subrSubOrDivOrMod(calc) {
  def subr(args) {
    def x := safeCar(args)
    def y := safeCar(safeCdr(args))
    if (x.tag() != "num" || y.tag() != "num") {
      return makeError("wrong type")
    }
    return makeNum(calc(x.data(), y.data()))
  }
  return subr
}
def calcSub(x, y) { return x - y }
def subrSub := subrSubOrDivOrMod(calcSub)
def calcDiv(x, y) { return x / y }
def subrDiv := subrSubOrDivOrMod(calcDiv)
def calcMod(x, y) { return x % y }
def subrMod := subrSubOrDivOrMod(calcMod)

addToEnv(sym_t, sym_t, g_env)
addToEnv(makeSym("car"), makeSubr(subrCar), g_env)
addToEnv(makeSym("cdr"), makeSubr(subrCdr), g_env)
addToEnv(makeSym("cons"), makeSubr(subrCons), g_env)
addToEnv(makeSym("eq"), makeSubr(subrEq), g_env)
addToEnv(makeSym("atom"), makeSubr(subrAtom), g_env)
addToEnv(makeSym("numberp"), makeSubr(subrNumberp), g_env)
addToEnv(makeSym("symbolp"), makeSubr(subrSymbolp), g_env)
addToEnv(makeSym("+"), makeSubr(subrAdd), g_env)
addToEnv(makeSym("*"), makeSubr(subrMul), g_env)
addToEnv(makeSym("-"), makeSubr(subrSub), g_env)
addToEnv(makeSym("/"), makeSubr(subrDiv), g_env)
addToEnv(makeSym("mod"), makeSubr(subrMod), g_env)

while (true) {
  print("> ")
  def line := stdin.readLine()
  if (line == null) {
    break
  }
  println(printObj(eval(read(line)[0], g_env)))
}
