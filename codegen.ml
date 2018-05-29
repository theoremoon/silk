open Llvm

let llvm_ctx = global_context ()
let llvm_module = create_module llvm_ctx "silk"

(* frequently used type *)
let void_t = void_type llvm_ctx
let i32_t = i32_type llvm_ctx
let i8_t = i8_type llvm_ctx

let create_entry_block =
  let main_t = function_type void_t [||] in
  let main_f = define_function "main" main_t llvm_module in
  let entry = entry_block main_f in
  builder_at_end llvm_ctx entry

let decl_print =
  let printf_t = var_arg_function_type i32_t [| pointer_type i8_t |] in
  declare_function "printf" printf_t llvm_module

let print_int x printf llvm_builder =
  let const_x = const_int i32_t x in
  let print_s = build_global_stringptr "%d\n" "" llvm_builder in
  let _ = build_call printf [| print_s; const_x |] "" llvm_builder in
  ()

let return_void llvm_builder =
  build_ret_void llvm_builder

let dump_module () =
  Llvm.dump_module llvm_module

let assert_valid_module () =
  Llvm_analysis.assert_valid_module llvm_module

let write_bitcode out =
  Llvm_bitwriter.write_bitcode_file llvm_module out
