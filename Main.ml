
open Common
open AST
open ISRTable
open Lexer
open Lexing
open Error

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  pos.pos_fname ^ ": line " ^ (string_of_int pos.pos_lnum) ^
  ", offset: " ^ (string_of_int (pos.pos_cnum - pos.pos_bol + 1))

let rec print_hd lst =
  match lst with
  | [] -> ()
  | (str, _) :: tl -> begin print_string (str ^ nl); print_hd tl; end

let loop filename =
  let inBuffer = open_in filename in
  let lexbuf = Lexing.from_channel inBuffer in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try 
    let res = Parser.parseVectors Lexer.lexVectors lexbuf in
    match res with
      | None -> print_string "Not accepted!"
      | Some (p) -> begin 
        print_string ("Parsing of " ^ filename ^ " succeeded." ^ nl);
        match p with
          | Assoc(x) -> let _ = vectors_to_vt x in ()
          | _        -> print_string "Error"
        end; 
  with
    | Error.SyntaxError msg -> print_string ("Parsing error: " ^ msg ^
                                             " in " ^ (print_position lexbuf) ^ nl)
    | Parser.Error -> print_string ("Parsing error in " ^ (print_position lexbuf) ^ nl)
    | StructureError msg -> print_string ("Parsing error: " ^ msg ^ nl)
    
let () =
  let in_file = Sys.argv.(1) in
    loop in_file
    