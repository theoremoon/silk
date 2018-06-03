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
let rec eval_exp exp func builder =
  match exp with
  |Int v -> const_int i32_t v
  |Neg exp1 -> 
      let v1 = eval_exp exp1 func builder in
      build_neg v1 "name" builder
  |Add (exp1, exp2) ->
      let v1 = eval_exp exp1 func builder in
      let v2 = eval_exp exp2 func builder in
      build_add v1 v2 "name" builder
  |Sub (exp1, exp2) ->
      let v1 = eval_exp exp1 func builder in
      let v2 = eval_exp exp2 func builder in
      build_sub v1 v2 "name" builder
  |Mul (exp1, exp2) ->
      let v1 = eval_exp exp1 func builder in
      let v2 = eval_exp exp2 func builder in
      build_mul v1 v2 "name" builder
  |Div (exp1, exp2) ->
      let v1 = eval_exp exp1 func builder in
      let v2 = eval_exp exp2 func builder in
      build_sdiv v1 v2 "name" builder (*signed div*)
  |Assign (name, exp1) ->
      let v1 = eval_exp exp1 func builder in
      let store = build_alloca i32_t name builder in
      let _ = build_store v1 store builder in
      Hashtbl.add env name store;
      v1
  |Var (name) -> build_load (Hashtbl.find env name) "" builder
  |Call (name, args) ->
      begin
        if name = "print" then
          let v1 = eval_exp (List.hd args) func builder in
          let print = match lookup_function "printf" llvm_module with
          |Some(f) -> f
          |None -> raise (SilkError "program error")
          in
        let print_s = build_global_stringptr "%d\n" "" builder in
        build_call print [|print_s; v1|] "" builder
        else
          let args = List.map (fun arg -> eval_exp arg func builder) args in
          match lookup_function name llvm_module with
        |Some(f) -> build_call f (Array.of_list args) "" builder
        |None -> raise (SilkError ("function [" ^ name ^ "] does not exist"))
      end
  |If (cond, then_exp, else_exp) ->
      begin
        let cond_val = eval_exp cond func builder in
        let cond_bool = build_icmp Icmp.Ne cond_val (const_int i32_t 0) "" builder in
        let then_block = append_block llvm_ctx "then" func in
        let else_block = append_block llvm_ctx "else" func in
        let merge_block = append_block llvm_ctx "merge" func in

        let then_builder = builder_at_end llvm_ctx then_block in
        let then_ret = eval_exp then_exp func then_builder in
        build_br merge_block then_builder |> ignore;

        let else_builder = builder_at_end llvm_ctx else_block in
        let else_ret = eval_exp else_exp func else_builder in
        build_br merge_block else_builder |> ignore;

        let merge_builder = builder_at_end llvm_ctx merge_block in
        let merge_val = build_phi [(then_ret, then_block); (else_ret, else_block)] "" merge_builder in

        build_cond_br cond_bool then_block else_block builder |> ignore;
        position_at_end merge_block builder;
        merge_val
      end
  |MultiExpr (exprs) ->
      let eval_in x e = eval_exp e func builder in
      List.fold_left eval_in (const_int i32_t 0) exprs

(* eval statement and return unit *)
let rec eval_stmt stmt func builder =
  match stmt with
  |Exp exp -> eval_exp exp func builder |> ignore
  |Defun (name, arg_names, body) ->
      if name = "main" then
        begin
          (* entry point *)
          let main_t = function_type void_t [||] in
          let main_f = define_function "main" main_t llvm_module in
          let entry = entry_block main_f in
          let builder = builder_at_end llvm_ctx entry in
          let _ = eval_exp body main_f builder in
          build_ret_void builder |> ignore
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
          let ret = eval_exp body f builder in
          build_ret ret builder |> ignore
        end
    
(* apply list of statements and return unit *)
and eval_stmts stmts func builder =
  match stmts with
  |stmt :: remained ->
  begin
    let last_exp = eval_stmt stmt func builder in
    match remained with
    |[] -> last_exp
    |_ -> eval_stmts remained func builder
  end
  |[] -> ()   (* dummy value *)

(* create LLVM IR code from program *)
let codegen stmts =
  (* declare builtin function *)
  let printf_t = var_arg_function_type i32_t [| pointer_type i8_t |] in
  let _ = declare_function "printf" printf_t llvm_module in

  (* create main functon and insert stmts *)
  let dummy_builder = Llvm.builder llvm_ctx in
  let dummy_func = const_int i32_t 0 in
  eval_stmts stmts dummy_func dummy_builder;
  llvm_module (* return *)


