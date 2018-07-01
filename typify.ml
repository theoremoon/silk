open Typ
open Syntax
open Error

let typeof exp =
  match exp with
  |IntT (_, t) -> t
  |NegT (_, t) -> t
  |CallT (_, _, t) -> t
  |AssignT (_, _, t) -> t
  |VarT (_, t) -> t
  |IfT (_, _, _, t) -> t
  |MultiExprT (_, t) -> t

let typeof_stmt stmt =
  match stmt with
  |ExpT (exp, t) -> t
  |DefunT (_, _, _, t) -> t

let rec typify_exp exp =
  match exp with
  |Int(v) -> IntT(v, Int)
  |_ -> raise (SilkError "Unimplemented typify")

let rec typify_stmt stmt =
  match stmt with
  |Exp (exp) ->
      let typed = typify_exp exp in
      ExpT(typed, typeof typed)
  |_ -> raise (SilkError "Unimplemented typify")

let rec typify_stmts stmts =
  match stmts with
  |stmt::xs -> (typify_stmt stmt)::(typify_stmts xs)
  |[] -> []

let typify stmts =
  typify_stmts stmts
