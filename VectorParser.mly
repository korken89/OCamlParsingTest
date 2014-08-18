%token <string> STRING
%token <string> ID
%token <int>    INT
%token PRIO STACK_ID CORE_VECTORS VENDOR_VECTORS
%token KERNEL RESV OVERRD FREE USED LC RC COMMA COLON EOF

%{
  open AST 
  open Error
%}

%start <AST.vector_table option> parseVectors

%%

parseVectors:
  | EOF                { None   }
  | v = base_structure { Some v }
  ;
  
base_structure:
  | pri = get_prio; COMMA; sid = get_stackid; COMMA; cv = get_corev; COMMA; vv = get_vendorv; {
      {
        max_priorities = pri;
        vector_table = sid :: cv @ vv;
      }
    }
  ;

get_prio:
  | PRIO; COLON; pri = INT { pri                                                         }
  | err = na_mid; COLON    { raise (SyntaxError ("Unexpected identifier used: '" ^ err ^
                                                 "', expected 'isr_max_priorities'"))    }
  ;

get_stackid:
  | STACK_ID; COLON; sid = STRING { (K, sid)                                                    }
  | err = na_mid; COLON           { raise (SyntaxError ("Unexpected identifier used: '" ^ err ^
                                                        "', expected 'stack_end_identifier'"))  }
  ;

get_corev:
  | CORE_VECTORS; COLON; LC; vecs = get_vectors; RC { vecs                                                        }
  | err = na_mid; COLON                             { raise (SyntaxError ("Unexpected identifier used: '" ^ err ^
                                                                          "', expected 'core_isr_vectors'"))      }
  ;

get_vendorv:
  | VENDOR_VECTORS; COLON; LC; vecs = get_vectors; RC { vecs                                                        }
  | err = na_mid; COLON                               { raise (SyntaxError ("Unexpected identifier used: '" ^ err ^
                                                                            "', expected 'vendor_isr_vectors'"))    }
  ;

get_vectors:
  | vecs = separated_list(COMMA, get_vector) { vecs }
  ;  
  
get_vector:
  | id = get_id; COLON; v = value  { (v, id)                                                      }
  | get_id; COLON; err = na_vector { raise (SyntaxError ("Unexpected value: '" ^ err ^ "'"))      }
  | err = na_id; COLON             { raise (SyntaxError ("Unexpected identifier: '" ^ err ^ "'")) }
  ;

get_id:
  | id = ID;  { id               }
  | id = INT; { string_of_int id }

value:
  | KERNEL { K }
  | RESV   { R }
  | OVERRD { O }
  | FREE   { F }
  | USED   { U }
  ;

na_vector:
  | s = STRING { s               }
  | id = ID    { id              }
  | i = INT    { string_of_int i }
  ;

na_id:
  | s = STRING { "String: \"" ^ s ^ "\"" }
  | KERNEL     { "kernel"                }
  | RESV       { "reserved"              }
  | OVERRD     { "overridable"           }
  | FREE       { "free"                  }
  | USED       { "used"                  }
  ;
  
na_mid:
  | id = ID    { id }
  | na = na_id { na }
  ;  
