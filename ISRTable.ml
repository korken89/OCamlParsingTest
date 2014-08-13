open Common
open AST
open Error

type isr_type =
  | K (* reserved by the RTFM kernel                *)
  | R (* reserved by ARM                            *) 
  | O (* overridable but has default implementation *)
  | F (* free to use by the application             *)
  | U (* used by the application                    *)

type isr_definition = {
  name : string;
  isr_type : isr_type;
}

type isr_vector_table = {
  max_priorities : int;
  core_vector_table : isr_definition list;
  vendor_vector_table : isr_definition list;
}

let value_type value =
  match value with 
    | Assoc(_)     -> "Assoc"
    | ValueList(_) -> "List"
    | String(_)    -> "String"
    | Int(_)       -> "Int"
    | Float(_)     -> "Float"
    | Bool(_)      -> "Bool"
    | Null         -> "Null"

let check_priorities prio =
  if value_type prio = "Int" then
    let prio_int =
    match prio with
      | Int(x) -> x
      | _      -> raise (StructureError("Unexpected error"))
    in if prio_int > 0 then
      prio_int
    else
      raise (StructureError("isr_max_priorities error: Expected an positive value, received " ^ (string_of_int prio_int)))
  else
    raise (StructureError("isr_max_priorities error: Expected an Int, received " ^ value_type prio))

let json_to_ivt (input : (string * value) list) =
  let len = List.length input in
  if len == 3 then
    let (str1, val1) = List.nth input 0 and
        (str2, val2) = List.nth input 1 and
        (str3, val3) = List.nth input 2 in
    if str1 = "isr_max_priorities" && str2 = "core_isr_vectors" && str3 = "vendor_isr_vectors" then
      
      print_string ("OK!" ^ nl ^ (value_type val1) ^ nl ^ (value_type val2) ^ nl ^ (value_type val3) ^ nl)
    else
      raise (StructureError("The correct objects were not found."))
  else
    raise (StructureError ("The number of objects in the array is wrong, expected 3 got " ^
      (string_of_int len)))