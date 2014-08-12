
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

let json_to_ivt (input : (string * value) list) =
  let len = List.length input in
  if len == 3 then
    let (str1, val1) = List.nth input 0 and
        (str2, val2) = List.nth input 1 and
        (str3, val3) = List.nth input 2 in
    if str1 = "isr_max_priorities" && str2 = "core_isr_vectors" && str3 = "vendor_isr_vectors" then
      print_string "OK!"
    else
      raise (StructureError("The correct objects were not found."))
  else
    raise (StructureError ("The number of objects in the array is wrong, expected 3 got " ^
      (string_of_int len)))

let loop filename =
  let inBuffer = open_in filename in
  let lexbuf = Lexing.from_channel inBuffer in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try 
    let res = Parser.prog Lexer.read lexbuf in
    match res with
      | None -> print_string "Not accepted!"
      | Some (p) -> begin 
        print_string ("Parsing of " ^ filename ^ " succeeded." ^ nl);
        match p with
          | Assoc(x) -> json_to_ivt x
          | _        -> print_string "Error"
        end; 
  with
    | Lexer.SyntaxError msg -> print_string ("Parsing error: " ^ msg ^ " in " ^ (print_position lexbuf))
    | Parser.Error -> print_string ("Parsing error in " ^ (print_position lexbuf))
    
let () =
  let in_file = Sys.argv.(1) in
    loop in_file
    