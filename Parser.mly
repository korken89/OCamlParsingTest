%token <int>    INT
%token <float>  FLOAT
%token <string> ID
%token <string> STRING
%token <bool>   BOOLVAL
%token NULL LB RB LC RC COMMA COLON EOF

%{
  open AST 
%}

%start prog

%type <AST.prog option> prog

%%

prog:
  | value* EOF { Some Prog ($2) }

value:
  | LC obj RC  {  }  (* Objects *)
  | LB list LC {  }
  | STRING COLON value {  }