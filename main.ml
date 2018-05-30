open Codegen
open Syntax

exception SilkError of string

let rec eval_exp exp llvm_builder =
  match exp with
  |Int v -> Llvm.const_int i32_t v
  |Add (exp1, exp2) ->
      let v1 = eval_exp exp1 llvm_builder in
      let v2 = eval_exp exp2 llvm_builder in
      Llvm.build_add v1 v2 "name" llvm_builder


(* main *)
let () =
  (* prepare llvm ir *)
  let llvm_builder = create_entry_block in
  let printf_f = decl_print in

  (* parse input *)
  let program = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let result =
    match program with
    |Exp exp -> eval_exp exp llvm_builder
  in
  let _ = print_int result printf_f llvm_builder in
  let _ = return_void llvm_builder in
  (* validation llvm ir  *)
  Codegen.assert_valid_module ();

  (* output llvm ir *)
  let _ =
    if Array.length Sys.argv > 1
    then 
      let oc = open_out Sys.argv.(1) in
      Codegen.write_bitcode_to_channel oc |> ignore;
      close_out oc;
      ()
    else 
      Codegen.dump_module ()
  in
  ()

