
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

type vector_base =
  | AssocPrio    of int
  | AssocStack   of string
  | AssocCoreVec of (isr_type * string) list
  | AssocVendVec of (isr_type * string) list

type top =
  | Assoc of (vector_base list)

(**
ISR vector table holder.
*)

type vector_table = {
  max_priorities : int;
  vector_table : (isr_type * string) list;
}


let ist_type_to_string = function
  | K -> "K"
  | R -> "R"
  | O -> "O"
  | F -> "F"
  | U -> "U"