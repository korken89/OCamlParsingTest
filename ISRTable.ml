open Common
open AST
open Error

(**
ISR vector table holder.
*)

type vector_table = {
  max_priorities : int;
  vector_table : (isr_type * string) list;
}

(**
Converts the vector_table to the RTFM format.

@param vt Input vector_table.

@return The converted table.
*)
let get_RTFM_vector_table vt = vt.vector_table

(**
Converts the vector_table to the RTFM format.

@param vt Input vector_table.

@return The converted max priorities.
*)
let get_RTFM_max_prio vt = vt.max_priorities

(**
Converts a value_type to String

@param value Input value.

@return The converted string.
*)
let value_type value =
  match value with
    | Assoc(_)     -> "Assoc"
    | String(_)    -> "String"
    | Int(_)       -> "Int"
    | ISR(_)       -> "ISR"

(**
Checks the value of isr_max_priorities.

@param prio Input value.

@raise StructureError Exception if there was an error.
@return The max number of priorities.
*)
let check_priorities prio =
  let prio_int = match prio with
    | Int(x) -> x
    | _      -> raise (StructureError("isr_max_priorities error: Expected an Int, received " ^ value_type prio))
  in
    if prio_int > 0 then
      prio_int
    else
      raise (StructureError("isr_max_priorities error: Expected a positive value, received " ^ (string_of_int prio_int)))

(**
Converts an Assoc list to the final representation for RTFM.

@param err   Error string for the exception.
@param vecs  Assoc object from parser.

@raise StructureError Exception if there was an error.
@return The max number of priorities.
*)
let check_vectors err vec =
  let val_to_isr err (id, value) =
    match value with
      | ISR(x) -> (x, id)
      | _      -> raise (StructureError(err ^ " error: Expected an ISR, received " ^ value_type value))
  in 
  let rec assoc_list_to_isr err vecs =
    match vecs with
      | []       -> []
      | hd :: tl -> (val_to_isr err hd) :: (assoc_list_to_isr err tl)
  in
  let vec_lst = match vec with
    | Assoc(x) -> x
    | _        -> raise (StructureError(err ^ " error: Expected an Assoc List, received " ^ value_type vec))
  in
    if List.length vec_lst > 0 then
      assoc_list_to_isr err vec_lst
    else
      raise (StructureError(err ^ " error: Expected an Assoc list, received empty List."))

(**
Converts the stack-pointer field to the final representation for RTFM.

@param value  String value.

@raise StructureError Exception if there was an error.
@return The max number of priorities.
*)
let check_stack_pointer value =
  match value with
    | String(str) -> (K, str)
    | _           -> raise (StructureError("stack_end_identifier error: Expected a String, received " ^ value_type value))

(**
Main function for convering a vector file to the final representation for RTFM.

@param in_value  The output from the parser.

@raise StructureError Exception if there was an error.
@return The finished vector_table.
*)
let parsed_vectors_to_vt in_vector =
  match in_vector with
    | Assoc(input) -> 
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
    | _            -> raise (StructureError("The correct structure were not found."))
      