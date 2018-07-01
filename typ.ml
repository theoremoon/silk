type typ =
  |Unit
  |Int
  |Bool
  |Fun of typ * typ

let rec string_of_type t =
  match t with
  |Unit -> "Unit"
  |Int -> "Int"
  |Bool -> "Bool"
  |Fun (a, r) -> (string_of_type a)^"->"^(string_of_type r)

