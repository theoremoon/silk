open Codegen

let _ =
  let llvm_builder = create_entry_block in
  let printf_f = decl_print in
  print_int 100 printf_f llvm_builder;
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

