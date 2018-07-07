open Error

type typ =
  |UnitT
  |IntT
  |BoolT
  |VarT of string
  |FunT of typ * typ

let arg_type funt =
  match funt with
  |FunT(argt, _) -> argt
  |_ -> raise (TypeError "function type requried")

let ret_type funt =
  match funt with
  |FunT(_, rett) -> rett
  |_ -> raise (TypeError "function type requried")

let rec string_of_type t =
  match t with
  |UnitT -> "Unit"
  |IntT -> "Int"
  |BoolT -> "Bool"
  |VarT(name) -> name
  |FunT(a, r) -> (string_of_type a)^"->"^(string_of_type r)

