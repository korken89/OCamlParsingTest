
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
  | Assoc     of (string * value) list
  | String    of string
  | Int       of int
  | ISR       of isr_type

let ist_type_to_string = function
  | K -> "K"
  | R -> "R"
  | O -> "O"
  | F -> "F"
  | U -> "U"