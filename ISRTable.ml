open Common
open AST
open Error

type isr_vector_table = {
  max_priorities : int;
  vector_table : (isr_type * string) list;
}


let value_type value =
  match value with
    | Assoc(_)     -> "Assoc"
    | String(_)    -> "String"
    | Int(_)       -> "Int"
    | ISR(_)       -> "ISR"


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
    | ISR(x) -> (x, id)
    | _      -> raise (StructureError(err ^ " error: Expected an ISR, received " ^ value_type value))


let rec assoc_list_to_isr err vecs =
  match vecs with
    | []       -> []
    | hd :: tl -> (val_to_isr err hd) :: (assoc_list_to_isr err tl)


let check_vectors err vec =
  let vec_lst = match vec with
    | Assoc(x) -> x
    | _        -> raise (StructureError(err ^ " error: Expected an Assoc List, received " ^ value_type vec))
  in
    if List.length vec_lst > 0 then
      assoc_list_to_isr err vec_lst
    else
      raise (StructureError(err ^ " error: Expected an Assoc list, received empty List."))


let check_stack_pointer value =
  match value with
    | String(str) -> (K, str)
    | _           -> raise (StructureError("stack_end_identifier error: Expected a String, received " ^ value_type value))


let vectors_to_vt (input : (string * value) list) =
  let len = List.length input in
  if len == 4 then
    let (str1, val1) = List.nth input 0 and
        (str2, val2) = List.nth input 1 and
        (str3, val3) = List.nth input 2 and
        (str4, val4) = List.nth input 3 in
    if str1 = "isr_max_priorities" &&
       str2 = "stack_end_identifier" &&
       str3 = "core_isr_vectors" &&
       str4 = "vendor_isr_vectors" then
      let prio = check_priorities val1 in
      let core_vectors = check_vectors "core_isr_vectors" val3 in
      let vendor_vectors = check_vectors "vendor_isr_vectors" val4 in
      {
        max_priorities = prio;
        vector_table = (check_stack_pointer val2) :: core_vectors @ vendor_vectors;
      }
    else
      raise (StructureError("The correct objects were not found."))
  else
    raise (StructureError ("The number of objects in the vector file is wrong, expected 4 got " ^
      (string_of_int len)))
      