open AST

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
      