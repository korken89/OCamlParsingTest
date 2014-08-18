
open Common
open AST
open ISRTable
open Lexing
open Error

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  pos.pos_fname ^ ": line " ^ (string_of_int pos.pos_lnum) ^
  ", offset: " ^ (string_of_int (pos.pos_cnum - pos.pos_bol + 1))

let loop filename =
  let inBuffer = open_in filename in
  let lexbuf = Lexing.from_channel inBuffer in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try 
    let res = VectorParser.parseVectors VectorLexer.lexVectors lexbuf in
    match res with
      | None -> print_string "Nothing read!"
      | Some (p) -> begin 
        print_string ("Parsing of " ^ filename ^ " succeeded." ^ nl);
        end; 
  with
    | Error.SyntaxError msg -> print_string ("Syntax error: " ^ msg ^
                                             " in " ^ (print_position lexbuf) ^ nl)
    | VectorParser.Error    -> print_string ("Syntax error in " ^ (print_position lexbuf) ^
                                    " - Is there a missplaced comma?" ^ nl)
    | StructureError msg    -> print_string ("Structure error: " ^ msg ^ nl)
    
let () =
  let in_file = Sys.argv.(1) in
    loop in_file
    