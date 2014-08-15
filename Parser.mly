%token <string> STRING
%token <int>    INT
%token KERNEL RESV OVERRD FREE USED LC RC COMMA COLON EOF

%{
  open AST 
%}

%start parseVectors

%type <AST.value option> parseVectors

%%

parseVectors:
  | EOF       { None }
  | v = value { Some v } ;

value:
  | LC; obj = obj_fields; RC             { Assoc obj    }
  | s = STRING                           { String s     }
  | i = INT                              { Int i        }
  | KERNEL                               { ISR K        }
  | RESV                                 { ISR R        }
  | OVERRD                               { ISR O        }
  | FREE                                 { ISR F        }
  | USED                                 { ISR U        } ;

obj_fields:
  obj = separated_list(COMMA, obj_field) { obj          } ;
  
obj_field:
  k = STRING; COLON; v = value           { (k, v)       } ;
