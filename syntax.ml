open Typ

type typ_exp = string

type exp =
  |Unit
  |Int of int
  |Bool of bool

  |Call of string * exp list
  |Assign of string * typ_exp option * exp
  |Var of string
  |If of exp * exp * exp
  |MultiExpr of exp list
  |Defun of string * (string * typ_exp option) list * typ_exp option * exp

type exp_t =
  |TUnit of typ
  |TInt of int * typ
  |TBool of bool * typ

  |TCall of string * exp_t list * typ
  |TAssign of string * exp_t * typ
  |TVar of string * typ
  |TIf of exp_t * exp_t * exp_t * typ
  |TMultiExpr of exp_t list * typ
  |TDefun of string * string list * exp_t * typ

let typeof exp =
  match exp with
  |TUnit(t) -> t
  |TInt (_, t) -> t
  |TBool(_, t) -> t
  |TCall (_, _, t) -> t
  |TAssign (_, _, t) -> t
  |TVar (_, t) -> t
  |TIf (_, _, _, t) -> t
  |TMultiExpr (_, t) -> t
  |TDefun (_, _, _, t) -> t

