
open Common

type isr_type =
  | K (* reserved by the RTFM kernel                *)
  | R (* reserved by ARM                            *) 
  | O (* overridable but has default implementation *)
  | F (* free to use by the application             *)
  | U (* used by the application                    *)

type value =
  | Assoc     of (string * value) list
  | String    of string
  | Int       of int
  | ISR       of isr_type
