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
let rec typify_func func_t args env =
  match func_t with
  |Fun (a, r) -> begin
    (* split arguments *)
    match args with
    |x::xs -> begin
      let typed_arg, env, = typify_exp x env in
      let arg_t = typeof typed_arg in
      (* check argument *)
      if a = arg_t then
        let ret_t, typed_args, env = typify_func r xs env in
        (ret_t, typed_arg::typed_args, env)
      (* or error *)
      else
        raise (SilkError ("TypeError: required "^(string_of_type a)^" but got "^(string_of_type arg_t)^"."))
    end
    |[] -> raise (SilkError "too few arguments")
  end
  |_ -> (func_t, [], env)

and typify_exp exp env =
  match exp with
  |Int(v) -> (IntT(v, Int), env)
  |Assign (name, exp) ->
      let typed_exp, env = typify_exp exp env in
      let t = typeof typed_exp in
      (AssignT(name, typed_exp, t), (name, t)::env)
  |Call (name, args) -> begin
    let func_t =
      match List.assoc_opt name builtin_optypes with
      |Some(func_t) -> func_t
      |None -> raise (SilkError "Unimplemented typify")
    in
    let ret_t, typed_args, env = typify_func func_t args env in
    (CallT(name, typed_args, ret_t), env)
  end
  |MultiExpr (exprs) -> begin
    let last_type = ref Unit in
    let env_ref = ref env in

    let typed_exprs = List.map (fun expr -> begin
      let typed, env = typify_exp expr !env_ref in
      last_type := typeof typed;
      env_ref := env;

      typed
    end) exprs in
    (MultiExprT(typed_exprs, !last_type), !env_ref)
  end
  |_ -> raise (SilkError "Unimplemented typify")

let rec typify_stmt stmt env =
  match stmt with
  |Exp (exp) ->
      let typed, env = typify_exp exp env in
      (ExpT(typed, typeof typed), env)
  |_ -> raise (SilkError "Unimplemented typify")

let rec typify_stmts stmts env =
  match stmts with
  |stmt::xs -> 
      let typed_stmt, env = typify_stmt stmt env in
      let typed_stmts, env = typify_stmts xs env in
      (typed_stmt::typed_stmts, env)
  |[] -> ([], env)

let typify stmts =
  let typed_stmts, _ = typify_stmts stmts [] in
  typed_stmts
