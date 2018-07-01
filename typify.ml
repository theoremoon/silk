open Typ
open Syntax
open Error

let builtin_optypes = [
  ("+",  Fun(Int, Fun(Int, Int)) );
  ("-",  Fun(Int, Fun(Int, Int)) );
  ("*",  Fun(Int, Fun(Int, Int)) );
  ("/",  Fun(Int, Fun(Int, Int)) );
  ("==", Fun(Int, Fun(Int, Bool)) );
  ("!=", Fun(Int, Fun(Int, Bool)) );
  ("<",  Fun(Int, Fun(Int, Bool)) );
  (">",  Fun(Int, Fun(Int, Bool)) );
  ("<=", Fun(Int, Fun(Int, Bool)) );
  (">=", Fun(Int, Fun(Int, Bool)) );
]

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

(* typify args and check types for func a -> b -> c ... *)
let rec typify_func func_t args =
  match func_t with
  |Fun (a, r) -> begin
    (* split arguments *)
    match args with
    |x::xs -> begin
      let typed_arg = typify_exp x in
      let arg_t = typeof typed_arg in
      (* check argument *)
      if a = arg_t then
        let ret_t, typed_args = typify_func r xs in
        (ret_t, typed_arg::typed_args)
      (* or error *)
      else
        raise (SilkError ("TypeError: required "^(string_of_type a)^" but got "^(string_of_type arg_t)^"."))
    end
    |[] -> raise (SilkError "too few arguments")
  end
  |_ -> (func_t, [])

and typify_exp exp =
  match exp with
  |Int(v) -> IntT(v, Int)
  |Assign (name, exp) ->
      let typed_exp = typify_exp exp in
      AssignT(name, typed_exp, typeof typed_exp)
  |Call (name, args) -> begin
    let func_t =
      match List.assoc_opt name builtin_optypes with
      |Some(func_t) -> func_t
      |None -> raise (SilkError "Unimplemented typify")
    in
    let ret_t, typed_args = typify_func func_t args in
    CallT(name, typed_args, ret_t)
  end
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
