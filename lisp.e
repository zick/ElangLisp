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

def read(var str) {
  str := skipSpaces(str)
  if (str.size() == 0) {
    return [makeError("empty input"), ""]
  } else if (str[0] == kRPar) {
    return [makeError("invalid syntax: " + str), ""]
  } else if (str[0] == kLPar) {
    return [makeError("noimpl"), ""]
  } else if (str[0] == kQuote) {
    return [makeError("noimpl"), ""]
  } else {
    return readAtom(str)
  }
}

while (true) {
  print("> ")
  def line := stdin.readLine()
  if (line == null) {
    break
  }
  println(read(line)[0].data())
}
