
open Common

(**
ISR type format of the RTFM-core compiler.
*)
type isr_type =
  | K (* reserved by the RTFM kernel                *)
  | R (* reserved by ARM                            *) 
  | O (* overridable but has default implementation *)
  | F (* free to use by the application             *)
  | U (* used by the application                    *)

(**
Value format of the vectors file.
*)
type value =
  | Assoc  of (string * value) list
  | String of string
  | Int    of int
  | ISR    of isr_type

(**
ISR vector table holder.
*)
type vector_table = {
  max_priorities : int;
  vector_table : (isr_type * string) list;
}

(**
Converts ISR type to a string.

@param Input isr_type.

@return The converted string.
*)
let isr_type_to_string = function
  | K -> "K"
  | R -> "R"
  | O -> "O"
  | F -> "F"
  | U -> "U"