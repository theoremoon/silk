type exp =
  |Int of int

  |BinOp of string * exp * exp
  |CmpOp of string * exp * exp

  |Neg of exp

  |Call of string * exp list
  |Assign of string * exp
  |Var of string
  |If of exp * exp * exp
  |MultiExpr of exp list

type stmt =
  |Exp of exp
  |Defun of string * string list * exp
