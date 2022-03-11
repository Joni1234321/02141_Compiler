module FM4FUNTypesAST

// GC ::= b -> C | GC [] GC
type gc = 
  | ThenStatement of (b * c)
  | ElseStatement of (gc * gc) 


// a ::= n | x | A[a] | a + a | a - a | a * a | a % a | a / a |
// -a | a ^ a | (a)
and a = 
  | Num of float
  | Var of string
  | ArrayVar of (string * a)
  | PlusExpr of (a * a)
  | MinusExpr of (a * a)
  | TimesExpr of (a * a)
  | ModExpr of (a * a)
  | DivExpr of (a * a)
  | UPlusExpr of (a)
  | UMinusExpr of (a)
  | PowExpr of (a * a)


// b ::= true | false | b & b | b | b | b && b | b || b | 
// !b | a = a | a != a | a > a | a >= a | a < a | a <= a | (b)
and b = 
  | Bool of bool
  | AndOp of (b * b)
  | OrOp of (b * b)
  | ForAllOp of (b * b)
  | ExistsOp of (b * b)
  | NotOp of b
  | EqualOp of (a * a)
  | NEqualOp of (a * a)
  | GreaterOp of (a * a)
  | GreaterEqualOp of (a * a)
  | LessOp of (a * a)
  | LessEqualOp of (a * a)

// c ::= x := a | A[a] := a | skip | C; C | if GC fi | do GC od | continue 
// | break | try C catch HC yrt | throw e
and c =
  | AssignStatement of (string * a) 
  | AssignArrayStatement of (string * a * a)
  | MultipleStatement of (c * c)
  | IfStatement of gc 
  | DoStatement of gc
  | SkipStatement