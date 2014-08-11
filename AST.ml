
open Common

type value =
  | Assoc     of (string * value) list
  | ValueList of value list
  | String    of string
  | Int       of int
  | Float     of float
  | Bool      of bool
  | Null

let rec print_value value =
  let rec intAssoc x = match x with
    | []             -> ""
    | (st, vl) :: tl -> (st ^ ": " ^ (print_value vl) ^ nl) ^ intAssoc tl
  in
  let rec intList x = match x with
    | []       -> ""
    | hd :: tl -> (print_value hd) ^ ", " ^ (intList tl)
  in
  match value with
    | Assoc (x)     -> intAssoc x
    | ValueList (x) -> "[ " ^ intList x ^ " ] " ^ nl
    | String (x)    -> x 
    | Int (x)       -> "Int: " ^ (string_of_int x)
    | Float (_)     -> "Float"
    | Bool (_)      -> "Bool"
    | Null          -> "Null"