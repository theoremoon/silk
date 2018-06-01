open Llvm
open Syntax

exception SilkError of string

(* global contexts *)
let llvm_ctx = global_context ()
let llvm_module = create_module llvm_ctx "silk"

(* variables *)
let env:(string, llvalue) Hashtbl.t = Hashtbl.create 10

(* frequently used type *)
let void_t = void_type llvm_ctx
let i32_t = i32_type llvm_ctx
let i8_t = i8_type llvm_ctx

(* entry point *)
let create_entry_block =
  let main_t = function_type void_t [||] in
  let main_f = define_function "main" main_t llvm_module in
  let entry = entry_block main_f in
  builder_at_end llvm_ctx entry


(* eval expression *)
let rec eval_exp exp llvm_builder =
  match exp with
  |Int v -> const_int i32_t v
  |Neg exp1 -> 
      let v1 = eval_exp exp1 llvm_builder in
      build_neg v1 "name" llvm_builder
  |Add (exp1, exp2) ->
      let v1 = eval_exp exp1 llvm_builder in
      let v2 = eval_exp exp2 llvm_builder in
      build_add v1 v2 "name" llvm_builder
  |Sub (exp1, exp2) ->
      let v1 = eval_exp exp1 llvm_builder in
      let v2 = eval_exp exp2 llvm_builder in
      build_sub v1 v2 "name" llvm_builder
  |Mul (exp1, exp2) ->
      let v1 = eval_exp exp1 llvm_builder in
      let v2 = eval_exp exp2 llvm_builder in
      build_mul v1 v2 "name" llvm_builder
  |Div (exp1, exp2) ->
      let v1 = eval_exp exp1 llvm_builder in
      let v2 = eval_exp exp2 llvm_builder in
      build_sdiv v1 v2 "name" llvm_builder (*signed div*)
  |Assign (name, exp1) ->
      let v1 = eval_exp exp1 llvm_builder in
      Hashtbl.add env name v1;
      v1
  |Var (name) -> Hashtbl.find env name
  |Call (name, exp1) ->
      let v1 = eval_exp exp1 llvm_builder in
      if name = "print" then
        let print = match lookup_function "printf" llvm_module with
          |Some(f) -> f
          |None -> raise (SilkError "program error")
        in
        let print_s = build_global_stringptr "%d\n" "" llvm_builder in
        build_call print [|print_s; v1|] "" llvm_builder
      else
        match lookup_function name llvm_module with
        |Some(f) -> build_call f [|v1|] "" llvm_builder
        |None -> raise (SilkError ("function [" ^ name ^ "] does not exist"))

(* eval statement *)
let eval_stmt stmt builder =
  match stmt with
  |Exp exp -> eval_exp exp builder
  |Defun (name, stmts) -> raise (SilkError "defun is not implemented yet")

(* apply list of statements. statement will return unit *)
let rec eval_stmts stmts builder =
  match stmts with
  |stmt :: remained ->
  begin
    eval_stmt stmt builder |> ignore;
    eval_stmts remained builder
  end
  |[] -> ()

(* create LLVM IR code from program *)
let codegen stmts =
  (* declare builtin function *)
  let printf_t = var_arg_function_type i32_t [| pointer_type i8_t |] in
  let _ = declare_function "printf" printf_t llvm_module in

  (* create main functon and insert stmts *)
  let main_builder = create_entry_block in
  eval_stmts stmts main_builder |> ignore;
  build_ret_void main_builder |> ignore;
  llvm_module (* return *)


