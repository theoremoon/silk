type exp =
  |Int of int
  |Add of exp * exp
  |Mul of exp * exp
  |Sub of exp * exp
  |Div of exp * exp
  |Neg of exp

type program =
  |Exp of exp
