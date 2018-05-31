type exp =
  |Int of int
  |Add of exp * exp
  |Mul of exp * exp
  |Sub of exp * exp
  |Div of exp * exp
  |Neg of exp
  |Assign of string * exp
  |Var of string

type statement =
  |Exp of exp
