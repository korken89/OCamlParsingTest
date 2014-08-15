{
  (* Tokens defined in Parser.mly *)

  open Parser   
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
let quote   = '"'
let str     = [^ '"']* 

(* Lexing rules *)
rule read = parse
  | white                  { read lexbuf }
  | newline                { next_line lexbuf; read lexbuf }
  | int                    { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | quote (str as s) quote { STRING(s) }
  | "kernel"               { KERNEL }
  | "reserved"             { RESV }
  | "overridable"          { OVERRD }
  | "free"                 { FREE }
  | "used"                 { USED }
  | '{'                    { LC }
  | '}'                    { RC }
  | ':'                    { COLON }
  | ','                    { COMMA }
  | "(*"                   { comments 0 lexbuf }
  | eof                    { EOF }
  | _                      { raise (SyntaxError ("Unexpected character: '" ^ Lexing.lexeme lexbuf ^ "'")) }

and comments level = parse
  | "*)"                   { if level = 0 then read lexbuf else comments (level-1) lexbuf }
  | "(*"                   { comments (level + 1) lexbuf }
  | newline                { next_line lexbuf; comments level lexbuf }
  | _                      { comments level lexbuf }
  | eof                    { raise (SyntaxError ("Comment not closed.")) }
