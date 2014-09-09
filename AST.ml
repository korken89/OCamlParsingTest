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
ISR vector table holder.
*)
type vector_table = {
  max_priorities : int;
  vector_table : (isr_type * string) list;
}
