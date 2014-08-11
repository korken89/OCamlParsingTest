%token <string> STRING
%token <int>    INT
%token <float>  FLOAT
%token TRUE FALSE NULL LB RB LC RC COMMA COLON EOF

%{
  open AST 
%}

%start prog

%type <AST.value option> prog

%%

prog:
  | EOF       { None }
  | v = value { Some v } ;

value:
  | LC; obj = obj_fields; RC             { Assoc obj    }
  | LB; vl = list_fields; RB             { ValueList vl }
  | s = STRING                           { String s     }
  | i = INT                              { Int i        }
  | x = FLOAT                            { Float x      }
  | TRUE                                 { Bool true    }
  | FALSE                                { Bool false   }
  | NULL                                 { Null         } ;

obj_fields:
  obj = separated_list(COMMA, obj_field) { obj          } ;
  
obj_field:
  k = STRING; COLON; v = value           { (k, v)       } ;

list_fields:
  vl = separated_list(COMMA, value)      { vl           } ;