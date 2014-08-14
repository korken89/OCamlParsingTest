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


let string_to_isr_type str =
  match str with
    | "kernel"      -> K
    | "reserved"    -> R
    | "overridable" -> O
    | "free"        -> F
    | "used"        -> U
    | _             -> raise (StructureError("isr_type error: Expected [kernel|reserved|overridable|free|used], received " ^ str))


let isr_type_to_string isr =
  match isr with
    | K -> "kernel"
    | R -> "reserved"
    | O -> "overridable"
    | F -> "free"
    | U -> "used"


let isr_definition_to_string isr =
  isr.name ^ ": " ^ (isr_type_to_string isr.isr_type)

    
let rec isr_definition_list_to_string isrs =
  match isrs with
    | []       -> ""
    | hd :: tl -> (isr_definition_to_string hd) ^ nl ^ (isr_definition_list_to_string tl)


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
  let prio_int = match prio with
    | Int(x) -> x
    | _      -> raise (StructureError("isr_max_priorities error: Expected an Int, received " ^ value_type prio))
  in
    if prio_int > 0 then
      prio_int
    else
      raise (StructureError("isr_max_priorities error: Expected a positive value, received " ^ (string_of_int prio_int)))


let val_to_isr err (id, value) =
  match value with
    | String(x) -> {name = id; isr_type = string_to_isr_type x}
    | _         ->  raise (StructureError(err ^ " error: Expected a String value, received " ^ value_type value))


let rec assoc_list_to_isr err vecs =
  match vecs with
    | []       -> []
    | hd :: tl -> (val_to_isr err hd) :: (assoc_list_to_isr err tl)


let check_vectors err vec =
  let vec_lst = match vec with
    | Assoc(x) -> x
    | _        -> raise (StructureError(err ^ " error: Expected an Assoc list, received " ^ value_type vec))
  in
    if List.length vec_lst > 0 then
      assoc_list_to_isr err vec_lst
    else
      raise (StructureError(err ^ " error: Expected an Assoc list, received empty List."))


let json_to_ivt (input : (string * value) list) =
  let len = List.length input in
  if len == 3 then
    let (str1, val1) = List.nth input 0 and
        (str2, val2) = List.nth input 1 and
        (str3, val3) = List.nth input 2 in
    if str1 = "isr_max_priorities" && str2 = "core_isr_vectors" && str3 = "vendor_isr_vectors" then
      let prio = check_priorities val1 in
      let core_vectors = check_vectors "core_isr_vectors" val2 in
      let vendor_vectors = check_vectors "vendor_isr_vectors" val3 in
      {
        max_priorities = prio;
        core_vector_table = core_vectors;
        vendor_vector_table = vendor_vectors;
      }
    else
      raise (StructureError("The correct objects were not found."))
  else
    raise (StructureError ("The number of objects in the vector file is wrong, expected 3 got " ^
      (string_of_int len)))
      