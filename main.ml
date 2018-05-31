open Codegen
open Syntax

exception SilkError of string

(* main *)
let () =
  (* prepare llvm ir *)
  let llvm_builder = create_entry_block in
  let printf_f = decl_print in

  let eval_stmt stmt =
    match stmt with
    |Exp exp -> Codegen.eval_exp exp llvm_builder
    |Defun (name, stmts) -> raise (SilkError "defun is not implemented yet")
  in

  let rec eval_stmts stmts =
    match stmts with
    |stmt :: remained ->
    begin
      eval_stmt stmt |> ignore;
      match remained with
      |[] -> eval_stmt stmt
      |_ -> eval_stmts remained
    end
    |[] -> raise (SilkError "Empty statement is not valid")
  in

  (* parse input *)
  let program = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let result = eval_stmts program in
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

