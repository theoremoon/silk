open Typ
open Syntax
open Error

type typenv = (string, typ) Hashtbl.t list (* ("x", IntT); ("y", VarT("'y")); ... *)
type typsubst = (string * typ) list (* ("'y", IntT); ("'z", VarT("'y")); ... *)

let builtin_optypes = [
  ("+",  FunT(IntT, FunT(IntT, IntT)) );
  ("-",  FunT(IntT, FunT(IntT, IntT)) );
  ("__neg",  FunT(IntT, IntT) );
  ("*",  FunT(IntT, FunT(IntT, IntT)) );
  ("/",  FunT(IntT, FunT(IntT, IntT)) );
  ("==", FunT(IntT, FunT(IntT, BoolT)) );
  ("!=", FunT(IntT, FunT(IntT, BoolT)) );
  ("<",  FunT(IntT, FunT(IntT, BoolT)) );
  (">",  FunT(IntT, FunT(IntT, BoolT)) );
  ("<=", FunT(IntT, FunT(IntT, BoolT)) );
  (">=", FunT(IntT, FunT(IntT, BoolT)) );
]

let typeof exp =
  match exp with
  |TInt (_, t) -> t
  |TCall (_, _, t) -> t
  |TAssign (_, _, t) -> t
  |TVar (_, t) -> t
  |TIf (_, _, _, t) -> t
  |TMultiExpr (_, t) -> t
  |TDefun (_, _, _, t) -> t

let add_typ name typ typenv =
  match typenv with
  |typtbl::xs -> begin
    let typtbl = Hashtbl.copy typtbl in
    Hashtbl.add typtbl name typ;
    typtbl::xs
  end
  |[] -> begin
    let typtbl = Hashtbl.create 10 in
    Hashtbl.add typtbl name typ;
    [typtbl]
  end

let lookup_scope name typenv =
  match Hashtbl.find_opt (List.hd typenv) name with
  |Some(typ) -> Some(typ)
  |None -> None


let rec lookup name typenv =
  match typenv with
  |typtbl::xs -> begin
    match Hashtbl.find_opt typtbl name with
    |Some(typ) -> Some(typ)
    |None -> lookup name xs
  end
  |[] -> None


let rec newtypevar name typenv =
  match lookup name typenv with
  |Some(_) -> newtypevar ("'"^name) typenv
  |None -> VarT(name)
        
let rec occurs var_name typ =
  if var_name = typ then true
  else
    match typ with
    |FunT(argt, rett) -> (occurs var_name argt) || (occurs var_name rett)
    |_ -> false

(* replace t with ty *)
let rec replace_typ (t: typ) name (ty: typ): typ =
  match t with
  |VarT(name') -> if name = name' then ty else t
  |FunT(argt, rett) -> FunT(replace_typ argt name ty, replace_typ rett name ty)
  |_ -> t

let apply_substs (t: typ) (s: typsubst): typ =
  List.fold_right (fun (name, ty) t -> replace_typ t name ty) s t

let rec unify_one (t1: typ) (t2: typ): typsubst =
  match (t1, t2) with
  |(VarT(name1), VarT(name2)) ->
      if name1 = name2 then []
      else [(name2, t1)]
  |(VarT(name), _) ->
      if occurs t1 t2 then raise (TypeError "not unifiable") 
      else [(name, t2)]
  |(_, VarT(name)) ->
      if occurs t2 t1 then raise (TypeError "not unifiable")
      else [(name, t1)]
  |(FunT(argt1, rett1), FunT(argt2, rett2)) ->
      unify [(argt1, argt2); (rett1, rett2)]
  |(_, _) ->
      if t1 = t2 then []
      else raise (TypeError ("type mismatched:"^(string_of_type t1)^", "^(string_of_type t2)))

and unify typs =
  match typs with
  |(t1, t2)::xs ->
      let substs = unify xs in
      let subst = unify_one (apply_substs t1 substs) (apply_substs t2 substs) in
      subst @ substs (* list concatenation *)
  |[] -> []

let subst_typenv (typenv:typenv) (subst:typsubst) :typenv =
  List.map
    (fun typtbl -> 
      Hashtbl.filter_map_inplace
        (fun name t ->
          let t = (apply_substs t subst) in
          Some(t))
        typtbl;
      typtbl)
    typenv 

let rec typify_exp exp typenv =
  match exp with
  |Int(v) -> (TInt(v, IntT), typenv)
  |Call(name, args) ->
      let f =
        match List.assoc_opt name builtin_optypes with
        |Some(f') -> f'  (* builtin *)
        |None -> begin
          match lookup name typenv with
          |Some(f') when is_funt f' -> f' (* user defined *)
          |_ -> raise (SilkError ("Undefined function: " ^ name))
        end 
      in
      (* unifying argument types and return ret_t *)
      let rec typify_call args f typenv =
        match args with
        |arg::xs ->
            let t = arg_type f in
            let arg_t, typenv = typify_exp arg typenv in
            let typenv = subst_typenv typenv (unify [(t, typeof arg_t)]) in
            let argts, r_t, typenv = typify_call xs (ret_type f) typenv in
            (arg_t::argts, r_t, typenv)
        |[] -> ([], f, typenv)
      in
      let argts, rett, typenv = typify_call args f typenv in
      (TCall(name, argts, rett), typenv)
  |If(cond, then_exp, else_exp) ->
      let cond_t, typenv = typify_exp cond typenv in
      let typenv = subst_typenv typenv (unify [(BoolT, typeof cond_t)]) in
      let then_t, typenv = typify_exp then_exp typenv in
      let else_t, typenv = typify_exp else_exp typenv in
      let typenv = subst_typenv typenv (unify [(typeof then_t, typeof else_t)]) in
      let then_t, typenv = typify_exp then_exp typenv in
      (TIf(cond_t, then_t, else_t, typeof then_t), typenv)
  |Var(name) -> begin
    match lookup name typenv with
    |Some(t) -> (TVar(name, t), typenv)
    |None -> raise (SilkError ("variable is undefined:"^name))
  end
  |Assign(name, exp) -> begin
    match lookup_scope name typenv with
    |Some(_) -> raise (SilkError ("variable is already defined: "^name))
    |None ->
        let expt, typenv = typify_exp exp typenv in
        let typenv = add_typ name (typeof expt) typenv in
        (TAssign(name, expt, typeof expt), typenv)
  end
  |MultiExpr(exprs) -> begin
    let typenv = (Hashtbl.create 10)::typenv in
    let rec typify_exprs exprs typenv =
      match exprs with
      |e::xs -> begin
        let e_t, typenv = typify_exp e typenv in
        match xs with
          |[] -> ([e_t], typeof e_t, typenv)
          |_ ->
            let e_ts, r_t, typenv = typify_exprs xs typenv in
            (e_t::e_ts, r_t, typenv)
      end
      |[] -> ([], UnitT, typenv)
    in
    let exprs_t, r_t, typenv = typify_exprs exprs typenv in
    let typenv = List.tl typenv in
    (TMultiExpr(exprs_t, r_t), typenv)
  end
  |Defun(name, arg_names, body) ->
      let scopeenv = (Hashtbl.create 10) in
      List.iter (fun argname ->
        let tyvar = newtypevar argname typenv in
        Hashtbl.add scopeenv argname tyvar) arg_names;
      let typenv = scopeenv::typenv in
      let bodyt, typenv = typify_exp body typenv in
      let argtypes = List.map (fun argname ->
        let argtype, _ = typify_exp (Var(argname)) typenv in
        typeof argtype) arg_names
      in
      let typenv = List.tl typenv in
      let funct = make_funt argtypes (typeof bodyt) in
      Hashtbl.add (List.hd typenv) name funct; 
      (TDefun(name, arg_names, bodyt, funct), typenv)

let typify exprs =
  let typed_expr, _ = typify_exp exprs [Hashtbl.create 10] in
  typed_expr
