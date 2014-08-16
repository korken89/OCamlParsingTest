{
  (* Tokens defined in Parser.mly *)

  open Parser   
  open Lexing
  open Common
  open Error
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
rule lexVectors = parse
  | white                  { lexVectors lexbuf                                                             }
  | newline                { next_line lexbuf; lexVectors lexbuf                                           }
  | int                    { INT (int_of_string (Lexing.lexeme lexbuf))                                    }
  | quote (str as s) quote { STRING(s)                                                                     }
  | "kernel"               { KERNEL                                                                        }
  | "reserved"             { RESV                                                                          }
  | "overridable"          { OVERRD                                                                        }
  | "free"                 { FREE                                                                          }
  | "used"                 { USED                                                                          }
  | id as i                { ID(i)                                                                         }
  | int id                 { raise (SyntaxError ("Unexpected identifier: '" ^ Lexing.lexeme lexbuf ^ "'")) }
  | '{'                    { LC                                                                            }
  | '}'                    { RC                                                                            }
  | ':'                    { COLON                                                                         }
  | ','                    { COMMA                                                                         }
  | "(*"                   { comments 0 lexbuf                                                             }
  | eof                    { EOF                                                                           }
  | _                      { raise (SyntaxError ("Unexpected character: '" ^ Lexing.lexeme lexbuf ^ "'"))  }

and comments level = parse
  | "*)"                   { if level = 0 then lexVectors lexbuf else comments (level-1) lexbuf }
  | "(*"                   { comments (level + 1) lexbuf                                        }
  | newline                { next_line lexbuf; comments level lexbuf                            }
  | _                      { comments level lexbuf                                              }
  | eof                    { raise (SyntaxError ("Comment not closed."))                        }
