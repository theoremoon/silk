open Llvm
open Syntax
open Error
open Typ

type llvm_context = {
  llvm_ctx : llcontext;
  llvm_mod : llmodule;  (* bad name! *)
  env : (string, llvalue) Hashtbl.t list;
  defined_funcs: (string * string) list;
  declared_funcs : (string * (string list * exp_t * typ)) list;
  builder : llbuilder;
  func : llvalue;
  namespace : string;
}


(* global contexts *)
let llvm_ctx = global_context ()
let llvm_module = create_module llvm_ctx "silk"

(* frequently used type *)
let void_t = void_type llvm_ctx
let i32_t = i32_type llvm_ctx
let i8_t = i8_type llvm_ctx
let bool_t = i1_type llvm_ctx

let dummy_llvalue = const_int i32_t 0

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

let rec lltype_of_type typ =
  match typ with
  |IntT -> i32_t
  |BoolT -> bool_t
  |UnitT -> i32_t  (* dirty: type of dummy_llvalue *)
  |_ -> raise (SilkError ("Unsupported type:"^(string_of_type typ)))

let rec codegen_defun fname arg_names types ret_t body ctx =
  let saved_builder = ctx.builder in
  let saved_namespace = ctx.namespace in
  let saved_func = ctx.func in
  let arg_types = Array.of_list (List.map lltype_of_type types) in
  let func_t = function_type (lltype_of_type ret_t) arg_types in
  let f_id = ctx.namespace ^ "$" ^ fname in
  let f = define_function f_id func_t ctx.llvm_mod in
  let entry = entry_block f in
  let builder = builder_at_end ctx.llvm_ctx entry in
  let ctx = { ctx with
    builder = builder;
    func = f;
    defined_funcs = (fname, f_id)::ctx.defined_funcs;
    env = (Hashtbl.create 16)::ctx.env;
    namespace = f_id;
  } in

  (* build parameter list *)
  let param_list = Array.to_list (Llvm.params f) in 
  let rec add_arg argnames argtypes params = 
    match (argnames, argtypes, params) with
    |(argname::an, argtype::at, param::ps) -> begin
      set_value_name argname param;
      let store = build_alloca argtype argname ctx.builder in
      build_store param store ctx.builder |> ignore;
      Hashtbl.add (List.hd ctx.env) argname store; (* warning: arugment name will be override *)
      add_arg an at ps
    end
    |([], [], []) -> ()
    |([], [void_t], [_]) -> ()
    |_ -> raise (SilkError ("Program Error")) 
  in
  add_arg arg_names (List.map lltype_of_type types) param_list;

  (* body and ret *)
  let ret, ctx = codegen_expr body ctx in
  build_ret ret builder |> ignore;

  (f, {
    ctx with
    builder = saved_builder;
    func = saved_func;
    env = List.tl ctx.env;
    namespace = saved_namespace;
  })

and codegen_expr expr ctx =
  match expr with
  |TUnit(_) -> (dummy_llvalue, ctx)
  |TInt(v, _) -> (const_int i32_t v, ctx)
  |TBool(v, _) -> (const_int bool_t (if v then 1 else 0), ctx)
  |TVar (name, _) -> begin
    match lookup name ctx.env with
    |Some(v) -> 
        let r = build_load v "" ctx.builder in
        (r, ctx)
    |None -> raise (SilkError ("Undefined variable: " ^ name))
  end
  |TAssign(name, exp, t) ->
      let v, ctx = codegen_expr exp ctx in
      let store = 
        match t with
        |IntT -> build_alloca i32_t name ctx.builder
        |BoolT -> build_alloca bool_t name ctx.builder
        |UnitT -> raise (SilkError "Unit type has not value")
        |_ -> raise (SilkError ("Unspported type: " ^ (string_of_type t)))
      in
      let _ = build_store v store ctx.builder in
      Hashtbl.add (List.hd ctx.env) name store;
      (v, ctx)
  |TCall(name, args, ret_t) -> begin
      let rec codegen_args args ctx =
        match args with
        |arg::xs ->
            let v, ctx = codegen_expr arg ctx in
            let vs, ts, ctx = codegen_args xs ctx in
            (v::vs, (typeof arg)::ts, ctx)
        |[] -> ([], [], ctx)
      in
      let rec build_fname name types =
        match types with
        |t::xs ->
            build_fname (name^"__"^(string_of_type t)) xs
        |[] -> name
      in
      (* eval args *)
      let args, types, ctx = codegen_args args ctx in
      (* build function name with types *) 
      let fname = build_fname name types in

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
      |None -> begin
        (* search functions *)
        match List.assoc_opt fname ctx.defined_funcs with
        |Some(f_id) -> begin
          match lookup_function f_id ctx.llvm_mod with
          |Some(f) -> 
            let r = build_call f (Array.of_list args) "" ctx.builder in
            (r, ctx)
          |None -> raise (SilkError "Program Error: function id missed")
        end
        |None -> begin
          match List.assoc_opt name ctx.declared_funcs with
          |Some(arg_names, body, ftype) ->
              let f, ctx = codegen_defun fname arg_names types ret_t body ctx in
              let r = build_call f (Array.of_list args) "" ctx.builder in
              (r, ctx)
          |None -> raise (SilkError ("undefined function (or does not match types): "^fname))
        end
      end
    end
  |TMultiExpr (exprs, _) ->
      let ctx_ref = ref {ctx with env = (Hashtbl.create 16)::ctx.env} in
      let ret_ref = ref (const_int i32_t 0) in
      List.iter (fun e ->
        let r, ctx = codegen_expr e !ctx_ref in
        ctx_ref := ctx;
        ret_ref := r) exprs;
      (!ret_ref, {!ctx_ref with env = List.tl (!ctx_ref).env})
  |TDefun(name, arg_names, body, t) ->
      if name = "main" then
        begin
          (* entry point *)
          let main_t = function_type void_t [||] in
          let main_f = define_function "main" main_t ctx.llvm_mod in
          let entry = entry_block main_f in
          let builder = builder_at_end ctx.llvm_ctx entry in
          let saved_namespace = ctx.namespace in
          let ctx = { ctx with builder = builder; func = main_f; env = (Hashtbl.create 16)::ctx.env; namespace = name } in
          let _, ctx = codegen_expr body ctx in
          build_ret_void builder |> ignore;
          (main_f, {ctx with builder = builder; env = List.tl ctx.env; namespace = saved_namespace})
        end
      else
        (dummy_llvalue, {ctx with declared_funcs = (name, (arg_names, body, t))::ctx.declared_funcs})
  |TIf (cond, then_exp, else_exp, _) ->
      begin
        let cond_val, ctx = codegen_expr cond ctx in
        let then_block = append_block ctx.llvm_ctx "then" ctx.func in
        let else_block = append_block ctx.llvm_ctx "else" ctx.func in
        let merge_block = append_block ctx.llvm_ctx "merge" ctx.func in

        let then_builder = builder_at_end ctx.llvm_ctx then_block in
        let then_ret, _ = codegen_expr then_exp {ctx with builder = then_builder; env = (Hashtbl.create 16)::ctx.env} in
        build_br merge_block then_builder |> ignore;

        let else_builder = builder_at_end ctx.llvm_ctx else_block in
        let else_ret, _ = codegen_expr else_exp {ctx with builder = else_builder; env = (Hashtbl.create 16)::ctx.env} in
        build_br merge_block else_builder |> ignore;

        let merge_builder = builder_at_end ctx.llvm_ctx merge_block in
        let merge_val = build_phi [(then_ret, then_block); (else_ret, else_block)] "" merge_builder in

        build_cond_br cond_val then_block else_block ctx.builder |> ignore;
        position_at_end merge_block ctx.builder;
        (merge_val, ctx)
      end

(* create LLVM IR code from program *)
let codegen exprs =
  (* create context *)
  let ctx = global_context () in
  let context = {
    llvm_ctx = ctx;
    llvm_mod = create_module llvm_ctx "silk";
    env = [];
    defined_funcs = [
      ("print__Int", "print__Int");
      ("print__Bool", "print__Bool");
    ];
    declared_funcs = [];
    builder = Llvm.builder ctx; (* dummy *)
    func = dummy_llvalue; (* dummy *)
    namespace = "";
  } in

  (* declare builtin function *)
  let print_int_t = function_type void_t [| i32_t |] in
  let _ = declare_function "print__Int" print_int_t context.llvm_mod in

  let print_bool_t = function_type void_t [| bool_t |] in
  let _ = declare_function "print__Bool" print_bool_t context.llvm_mod in
  let _, context = codegen_expr exprs context in

  context.llvm_mod; (* return *)

