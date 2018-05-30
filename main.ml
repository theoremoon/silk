open Codegen
open Syntax

(* output LLVM IR *)
(* print number x *)
let compile x = 
  let llvm_builder = create_entry_block in

  let printf_f = decl_print in
  print_int x printf_f llvm_builder;
  let _ = return_void llvm_builder in

  Codegen.assert_valid_module ();

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

exception SilkError of string

(* main *)
let () =
  let program = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let _ =
    match program with
    |Int v -> compile v
  in
  ()



