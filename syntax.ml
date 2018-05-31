type exp =
  |Int of int
  |Add of exp * exp
  |Mult of exp * exp

type program =
  |Exp of exp
