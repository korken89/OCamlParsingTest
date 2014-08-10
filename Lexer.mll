{
  (* Tokens and dependanceies *)
  type token =
    | NULL
    | BOOLVAL of bool
    | STRING of string
    | INT of int
    | FLOAT of float
    | ID of string
    | LB
    | RB
    | LC
    | RC
    | COMMA
    | COLON
    | EOF

  open Lexing
  open Common
  open Error
  
  exception SyntaxError of string 
}

(* Regular expressions *)
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id      = ['A'-'Z' 'a'-'z' '_']['0'-'9' 'A'-'Z' 'a'-'z' '_']*  
let digit   = ['0'-'9']+
let int     = '-'? ['0'-'9'] ['0'-'9']*
let frac    = '.' digit*
let exp     = ['e' 'E'] ['-' '+']? digit+
let float   = digit* frac exp?

(* Lexing rules *)
rule read = parse
  | white        { read lexbuf }
  | newline      { next_line lexbuf; read lexbuf }
  | int          { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float        { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"       { BOOLVAL(true) }
  | "false"      { BOOLVAL(false) }
  | '"'          { read_string (Buffer.create 20) lexbuf }
  | '{'          { LC }
  | '}'          { RC }
  | '['          { LB }
  | ']'          { RB }
  | ':'          { COLON }
  | ','          { COMMA }
  | "//"         { comment lexbuf }
  | eof          { EOF }
  | _            { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
  | '"'          { STRING (Buffer.contents buf) }
  | eof          { raise (SyntaxError ("String not closed.")) }
  | _            { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }

and comment = parse
  | newline      { next_line lexbuf; read lexbuf }
  | eof          { EOF }
  | _            { comment lexbuf }
