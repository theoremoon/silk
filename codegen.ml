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
      let store = build_alloca i32_t name llvm_builder in
      let _ = build_store v1 store llvm_builder in
      Hashtbl.add env name store;
      v1
  |Var (name) -> build_load (Hashtbl.find env name) "" llvm_builder
  |Call (name, args) ->
      if name = "print" then
        let v1 = eval_exp (List.hd args) llvm_builder in
        let print = match lookup_function "printf" llvm_module with
          |Some(f) -> f
          |None -> raise (SilkError "program error")
        in
        let print_s = build_global_stringptr "%d\n" "" llvm_builder in
        build_call print [|print_s; v1|] "" llvm_builder
      else
        let args = List.map (fun arg -> eval_exp arg llvm_builder) args in
        match lookup_function name llvm_module with
        |Some(f) -> build_call f (Array.of_list args) "" llvm_builder
        |None -> raise (SilkError ("function [" ^ name ^ "] does not exist"))

(* eval statement *)
let rec eval_stmt stmt builder =
  match stmt with
  |Exp exp -> eval_exp exp builder
  |Defun (name, arg_names, stmts) ->
      if name = "main" then
        begin
          (* entry point *)
          let main_t = function_type void_t [||] in
          let main_f = define_function "main" main_t llvm_module in
          let entry = entry_block main_f in
          let builder = builder_at_end llvm_ctx entry in
          let ret = eval_stmts stmts builder in
          build_ret_void builder |> ignore;
          ret
        end
      else
        begin
          (* declare function *)
          let arg_types = Array.of_list (List.map (fun x -> i32_t) arg_names) in
          let func_t = function_type i32_t arg_types in
          let f = define_function name func_t llvm_module in
          let entry = entry_block f in
          let builder = builder_at_end llvm_ctx entry in

          (* build parameter list *)
          let param_list = Array.to_list (Llvm.params f) in 
          let add_arg arg_name param = 
            begin
              set_value_name arg_name param;
              let store = build_alloca i32_t arg_name builder in
              build_store param store builder |> ignore;
              Hashtbl.add env arg_name store; (* warning: arugment name will be override *)
            end
          in
          let _ = List.map2 add_arg arg_names param_list in

          (* body and ret *)
          let ret = eval_stmts stmts builder in
          build_ret ret builder |> ignore;
          ret
        end
    
(* apply list of statements and return last evaluated value *)
and eval_stmts stmts builder =
  match stmts with
  |stmt :: remained ->
  begin
    let last_exp = eval_stmt stmt builder in
    match remained with
    |[] -> last_exp
    |_ -> eval_stmts remained builder
  end
  |[] -> const_int i32_t 0   (* dummy value *)

(* create LLVM IR code from program *)
let codegen stmts =
  (* declare builtin function *)
  let printf_t = var_arg_function_type i32_t [| pointer_type i8_t |] in
  let _ = declare_function "printf" printf_t llvm_module in

  (* create main functon and insert stmts *)
  let dummy_builder = Llvm.builder llvm_ctx in
  eval_stmts stmts dummy_builder |> ignore;
  llvm_module (* return *)


