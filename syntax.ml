type exp =
  |Int of int
  |Add of exp * exp

type program =
  |Exp of exp
