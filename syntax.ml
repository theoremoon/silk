open Typ

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

type exp_t =
  |IntT of int * typ

  |BinOpT of string * exp_t * exp_t * typ
  |CmpOpT of string * exp_t * exp_t * typ

  |NegT of exp_t * typ

  |CallT of string * exp_t list * typ
  |AssignT of string * exp_t * typ
  |VarT of string * typ
  |IfT of exp_t * exp_t * exp_t * typ
  |MultiExprT of exp_t list * typ

type stmt =
  |Exp of exp
  |Defun of string * string list * exp

type stmt_t =
  |ExpT of exp_t * typ
  |DefunT of string * string list * exp_t * typ
