open Typ

type exp =
  |Int of int

  |Call of string * exp list
  |Assign of string * exp
  |Var of string
  |If of exp * exp * exp
  |MultiExpr of exp list
  |Defun of string * string list * exp

type exp_t =
  |TInt of int * typ

  |TCall of string * exp_t list * typ
  |TAssign of string * exp_t * typ
  |TVar of string * typ
  |TIf of exp_t * exp_t * exp_t * typ
  |TMultiExpr of exp_t list * typ
  |TDefun of string * string list * exp_t * typ
