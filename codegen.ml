open Llvm
open Syntax

let llvm_ctx = global_context ()
let llvm_module = create_module llvm_ctx "silk"

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

(* declare print function *)
let decl_print =
  let printf_t = var_arg_function_type i32_t [| pointer_type i8_t |] in
  declare_function "printf" printf_t llvm_module

(* call print function for int *)
let print_int x printf llvm_builder =
  let print_s = build_global_stringptr "%d\n" "" llvm_builder in
  let _ = build_call printf [| print_s; x |] "" llvm_builder in
  ()

let return_void llvm_builder =
  build_ret_void llvm_builder

let rec eval_exp exp llvm_builder =
  match exp with
  |Int v -> const_int i32_t v
  |Add (exp1, exp2) ->
      let v1 = eval_exp exp1 llvm_builder in
      let v2 = eval_exp exp2 llvm_builder in
      build_add v1 v2 "name" llvm_builder
  |Mult (exp1, exp2) ->
      let v1 = eval_exp exp1 llvm_builder in
      let v2 = eval_exp exp2 llvm_builder in
      build_mul v1 v2 "name" llvm_builder

let dump_module () =
  Llvm.dump_module llvm_module

let assert_valid_module () =
  Llvm_analysis.assert_valid_module llvm_module

let write_bitcode_to_channel out_channel =
  Llvm_bitwriter.output_bitcode out_channel llvm_module 
