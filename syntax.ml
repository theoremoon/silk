type exp =
  |Int of int
  |Add of exp * exp
  |Mul of exp * exp
  |Sub of exp * exp
  |Div of exp * exp
  |Neg of exp
  |Call of string * exp list
  |Assign of string * exp
  |Var of string
  |If of exp * exp * exp
  |MultiExpr of exp list

type stmt =
  |Exp of exp
  |Defun of string * string list * exp
