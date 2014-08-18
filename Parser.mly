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
  | PRIO; COLON; pri = INT                            { pri }
  | err = not_allowed_mid; COLON { raise (SyntaxError ("Unexpected identifier used: '" ^ err ^ "'")) }
  ;

get_stackid:
  | STACK_ID; COLON; sid = STRING                     { (K, sid) }
  | err = not_allowed_mid; COLON { raise (SyntaxError ("Unexpected identifier used: '" ^ err ^ "'")) }
  ;

get_corev:
  | CORE_VECTORS; COLON; LC; vecs = get_vectors; RC   { vecs }
  | err = not_allowed_mid; COLON { raise (SyntaxError ("Unexpected identifier used: '" ^ err ^ "'")) }
  ;

get_vendorv:
  | VENDOR_VECTORS; COLON; LC; vecs = get_vectors; RC { vecs }
  | err = not_allowed_mid; COLON { raise (SyntaxError ("Unexpected identifier used: '" ^ err ^ "'")) }
  ;

get_vectors:
  | vecs = separated_list(COMMA, get_vector)          { vecs }
  ;  
  
get_vector:
  | id = ID; COLON; v = value   { (v, id)                                                           }
  | id = INT; COLON; v = value  { (v, string_of_int id)                                             }
  | ID; COLON; err = ID
  | INT; COLON; err = ID        { raise (SyntaxError ("Unexpected value: '" ^ err ^ "'"))           }
  | err = not_allowed_id; COLON { raise (SyntaxError ("Unexpected identifier used: '" ^ err ^ "'")) }
  ;

value:
  | KERNEL { K }
  | RESV   { R }
  | OVERRD { O }
  | FREE   { F }
  | USED   { U }
  ;

not_allowed_id:
  | s = STRING { "String : \"" ^ s ^ "\"" }
  | KERNEL     { "kernel"                 }
  | RESV       { "reserved"               }
  | OVERRD     { "overridable"            }
  | FREE       { "free"                   }
  | USED       { "used"                   }
  ;
  
not_allowed_mid:
  | s = STRING { s               }
  | id = ID    { id              }
  | i = INT    { string_of_int i }
  | KERNEL     { "kernel"        }
  | RESV       { "reserved"      }
  | OVERRD     { "overridable"   }
  | FREE       { "free"          }
  | USED       { "used"          }
  ;  
