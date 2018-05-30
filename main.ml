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
      Codegen.write_bitcode Sys.argv.(1) |> ignore
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
    |_ -> raise (SilkError "program error")
  in
  ()



