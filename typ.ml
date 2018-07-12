open Error

type typ =
  |UnitT
  |IntT
  |BoolT
  |VarT of string
  |FunT of typ * typ

let is_funt t =
  match t with
  |FunT(_, _) -> true
  |_ -> false

let arg_type funt =
  match funt with
  |FunT(argt, _) -> argt
  |_ -> raise (TypeError "function type requried")

let ret_type funt =
  match funt with
  |FunT(_, rett) -> rett
  |_ -> raise (TypeError "function type requried")

let make_funt argtypes rettype =
  let rec make_funt' argtypes rettype =
    match argtypes with
    |argt::xs ->
        FunT(argt, make_funt' xs rettype)
    |[] -> rettype
  in
  match argtypes with
  |[] -> make_funt' [UnitT] rettype
  |_ -> make_funt' argtypes rettype


let rec string_of_type t =
  match t with
  |UnitT -> "Unit"
  |IntT -> "Int"
  |BoolT -> "Bool"
  |VarT(name) -> name
  |FunT(a, r) -> (string_of_type a)^"->"^(string_of_type r)

