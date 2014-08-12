open Common

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
