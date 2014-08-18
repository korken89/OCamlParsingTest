open Common
open AST
open Error

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
Pretty prints the vector_table object.

@param vt Input vector_table.

@return The pretty string.
*)
let pretty_print_vector_table vt =
  let rec vec_str = function
    | []                -> ""
    | (isr, name) :: tl -> (isr_type_to_string isr) ^ ": " ^ name ^ nl ^ (vec_str tl)
  in
  "Max priorities: " ^ (string_of_int vt.max_priorities) ^ nl ^ nl ^
  "Vector table:" ^ nl ^ (vec_str vt.vector_table)
      