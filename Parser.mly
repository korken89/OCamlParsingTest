%token <string> STRING
%token <string> ID
%token <int>    INT
%token KERNEL RESV OVERRD FREE USED LC RC COMMA COLON EOF

%{
  open AST 
  open Error
%}

%start parseVectors

%type <AST.value option> parseVectors

%%

parseVectors:
  | EOF       { None   }
  | v = value { Some v } ;

value:
  | LC; obj = obj_fields; RC { Assoc obj }
  | s = STRING               { String s  }
  | i = INT                  { Int i     }
  | KERNEL                   { ISR K     }
  | RESV                     { ISR R     }
  | OVERRD                   { ISR O     }
  | FREE                     { ISR F     }
  | USED                     { ISR U     } ;

obj_fields:
  obj = separated_list(COMMA, obj_field) { obj } ;
  
obj_field:
  | id = ID; COLON; v = value   { (id, v)                                                           }
  | id = INT; COLON; v = value  { (string_of_int id, v)                                             }
  | ID; COLON; err = ID
  | INT; COLON; err = ID        { raise (SyntaxError ("Unexpected value: '" ^ err ^ "'"))           }
  | err = not_allowed_id; COLON { raise (SyntaxError ("Unexpected identifier used: '" ^ err ^ "'")) } ;

not_allowed_id:
  | s = STRING { "String : \"" ^ s ^ "\"" }
  | KERNEL     { "kernel"                 }
  | RESV       { "reserved"               }
  | OVERRD     { "overridable"            }
  | FREE       { "free"                   }
  | USED       { "used"                   }
