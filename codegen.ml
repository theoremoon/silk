open Llvm
open Syntax
open Error

type llvm_context = {
  llvm_ctx : llcontext;
  llvm_mod : llmodule;  (* bad name! *)
  env : (string, llvalue) Hashtbl.t list;
  builder : llbuilder;
  func : llvalue;
}


(* global contexts *)
let llvm_ctx = global_context ()
let llvm_module = create_module llvm_ctx "silk"

(* frequently used type *)
let void_t = void_type llvm_ctx
let i32_t = i32_type llvm_ctx
let i8_t = i8_type llvm_ctx

(* assoc list of binary operations *)
type op_t = 
  |UniOp of (llvalue -> string -> llbuilder -> llvalue)
  |BinOp of (llvalue -> llvalue -> string -> llbuilder -> llvalue)
  |CmpOp of Icmp.t

let builtin_ops = [
  ("__neg",  UniOp(build_neg));
  ("+",  BinOp(build_add));
  ("-",  BinOp(build_sub));
  ("*",  BinOp(build_mul));
  ("/",  BinOp(build_sdiv));
  ("==", CmpOp(Icmp.Eq));
  ("!=", CmpOp(Icmp.Ne));
  ("<",  CmpOp(Icmp.Slt));
  (">",  CmpOp(Icmp.Sgt));
  ("<=", CmpOp(Icmp.Sle));
  (">=", CmpOp(Icmp.Sge));
]

(* lookup name from context *)
let rec lookup name env =
  match env with
  |cur::paren -> begin
    match Hashtbl.find_opt cur name with
    |Some(v) -> Some(v)
    |None -> lookup name paren
  end
  |[] -> None

let rec codegen_expr expr ctx =
  match expr with
  |TInt(v, _) -> (const_int i32_t v, ctx)
  |TMultiExpr (exprs, _) ->
      let ctx_ref = ref {ctx with env = (Hashtbl.create 16)::ctx.env} in
      let ret_ref = ref (const_int i32_t 0) in
      List.iter (fun e ->
        let r, ctx = codegen_expr e !ctx_ref in
        ctx_ref := ctx;
        ret_ref := r) exprs;
      (!ret_ref, {!ctx_ref with env = List.tl (!ctx_ref).env})
  |_ -> raise Unimplemented

(* eval expression; returns pair of result and new context *)
let rec eval_exp exp ctx =
  match exp with
  |Int v -> (const_int i32_t v, ctx)
  |Assign (name, exp1) ->
      let v1, ctx = eval_exp exp1 ctx in
      let store = build_alloca i32_t name ctx.builder in
      let _ = build_store v1 store ctx.builder in
      Hashtbl.add (List.hd ctx.env) name store;
      (v1, ctx)
  |Var (name) -> begin
    match lookup name ctx.env with
    |Some(v) -> 
        let r = build_load v "" ctx.builder in
        (r, ctx)
    |None -> raise (SilkError ("Undefined variable: " ^ name))
  end
  |Call (name, args) -> begin
    let eval_args () =
      let ctx_ref = ref ctx in
      let args = List.map (fun arg ->
        let r, ctx = eval_exp arg !ctx_ref in
        ctx_ref := ctx;
            r) args in
      (args, !ctx_ref)
    in
    let args, ctx = eval_args () in
    match List.assoc_opt name builtin_ops with
    |Some(UniOp(build_uniop)) ->
      let r = build_uniop (List.hd args) "name" ctx.builder in
      (r, ctx)
    |Some(BinOp(build_binop)) ->
        (* arithmetic operators *)
        let r = build_binop (List.nth args 0) (List.nth args 1) "name" ctx.builder in
          (r, ctx)
    |Some(CmpOp(cmp_icmp)) ->
        (* compartors *)
        let r = build_icmp cmp_icmp (List.hd args) (List.nth args 1) "name" ctx.builder in
        (r, ctx)
    |None -> 
        (* general functions *)
        match lookup_function name ctx.llvm_mod with
        |Some(f) -> 
            let r = build_call f (Array.of_list args) "" ctx.builder in
          (r, ctx)
        |None -> raise (SilkError ("function [" ^ name ^ "] does not exist"))
  end
  |If (cond, then_exp, else_exp) ->
      begin
        let cond_val, ctx = eval_exp cond ctx in
        let then_block = append_block ctx.llvm_ctx "then" ctx.func in
        let else_block = append_block ctx.llvm_ctx "else" ctx.func in
        let merge_block = append_block ctx.llvm_ctx "merge" ctx.func in

        let then_builder = builder_at_end ctx.llvm_ctx then_block in
        let then_ret, _ = eval_exp then_exp {ctx with builder = then_builder; env = (Hashtbl.create 16)::ctx.env} in
        build_br merge_block then_builder |> ignore;

        let else_builder = builder_at_end ctx.llvm_ctx else_block in
        let else_ret, _ = eval_exp else_exp {ctx with builder = else_builder; env = (Hashtbl.create 16)::ctx.env} in
        build_br merge_block else_builder |> ignore;

        let merge_builder = builder_at_end ctx.llvm_ctx merge_block in
        let merge_val = build_phi [(then_ret, then_block); (else_ret, else_block)] "" merge_builder in

        build_cond_br cond_val then_block else_block ctx.builder |> ignore;
        position_at_end merge_block ctx.builder;
        (merge_val, ctx)
      end
  |MultiExpr (exprs) ->
      let ctx_ref = ref {ctx with env = (Hashtbl.create 16)::ctx.env} in
      let ret_ref = ref (const_int i32_t 0) in
      List.iter (fun e ->
        let r, ctx = eval_exp e !ctx_ref in
        ctx_ref := ctx;
        ret_ref := r) exprs;
      (!ret_ref, {!ctx_ref with env = List.tl (!ctx_ref).env})
  |Defun (name, arg_names, body) ->
      if name = "main" then
        begin
          (* entry point *)
          let main_t = function_type void_t [||] in
          let main_f = define_function "main" main_t ctx.llvm_mod in
          let entry = entry_block main_f in
          let builder = builder_at_end ctx.llvm_ctx entry in
          let ctx = { ctx with builder = builder; func = main_f; env = (Hashtbl.create 16)::ctx.env} in
          let _, ctx = eval_exp body ctx in
          build_ret_void builder |> ignore;
          (main_f, {ctx with builder = builder; env = List.tl ctx.env})
        end
      else
        begin
          (* declare function *)
          let arg_types = Array.of_list (List.map (fun x -> i32_t) arg_names) in
          let func_t = function_type i32_t arg_types in
          let f = define_function name func_t ctx.llvm_mod in
          let entry = entry_block f in
          let builder = builder_at_end ctx.llvm_ctx entry in
          let ctx = { ctx with builder = builder; func = f; env = (Hashtbl.create 16)::ctx.env } in

          (* build parameter list *)
          let param_list = Array.to_list (Llvm.params f) in 
          let add_arg arg_name param = 
            begin
              set_value_name arg_name param;
              let store = build_alloca i32_t arg_name ctx.builder in
              build_store param store ctx.builder |> ignore;
              Hashtbl.add (List.hd ctx.env) arg_name store; (* warning: arugment name will be override *)
            end
          in
          let _ = List.map2 add_arg arg_names param_list in

          (* body and ret *)
          let ret, ctx = eval_exp body ctx in
          build_ret ret builder |> ignore;
          (f, {ctx with builder = builder; env = List.tl ctx.env})
        end

(* create LLVM IR code from program *)
let codegen exprs =
  (* create context *)
  let ctx = global_context () in
  let context = {
    llvm_ctx = ctx;
    llvm_mod = create_module llvm_ctx "silk";
    env = [];
    builder = Llvm.builder ctx; (* dummy *)
    func = const_int i32_t 0; (* dummy *)
  } in

  (* declare builtin function *)
  let print_t = function_type void_t [| i32_t |] in
  let _ = declare_function "print" print_t context.llvm_mod in

  let _, context = codegen_expr exprs context in
  context.llvm_mod; (* return *)

