open Codegen
open Typify
open Syntax
open Typ

let rec string_of_typed program =
  match program with
  |TInt(v, _) -> (string_of_int v)^":Int"
  |TCall(name, args, rtype) -> name^"("^(String.concat ", " (List.map string_of_typed args))^"):"^(string_of_type rtype)
  |TAssign(name, exp, typ) -> name^"="^(string_of_typed exp)
  |TVar(name, typ) -> name^":"^(string_of_type typ)
  |TIf(cond, then_e, else_e, typ) -> "if "^(string_of_typed cond)^" { "^(string_of_typed then_e)^" } else { " ^(string_of_typed then_e)^" }:"^(string_of_type typ)
  |TMultiExpr(exprs, typ) -> "{\n"^(String.concat ",\n" (List. map string_of_typed exprs))^"\n}:"^(string_of_type typ)
  |TDefun(name, args, body, typ) -> (string_of_type typ)^"\ndef "^name^"("^(String.concat ", " args)^")\n"^(string_of_typed body)

(* main *)
let () =

  (* parse input *)
  let program = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in

  (* type check *)
  let typed_program = Typify.typify program in
  string_of_typed typed_program |> print_endline;
  (* codegen *)
  let llvm_module = Codegen.codegen typed_program in

  (* output llvm ir *)
  let _ =
    if Array.length Sys.argv > 1 then 
    begin
      (* assertion *)
      Llvm_analysis.assert_valid_module llvm_module;
      (* output bitcode to file *)
      let oc = open_out Sys.argv.(1) in
      Llvm_bitwriter.output_bitcode oc llvm_module |> ignore;
      close_out oc;
      ()
    end
    else 
      (* output ir to stderr *)
      Llvm.dump_module llvm_module
  in
  ()

