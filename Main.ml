
open Common
open AST

let main () =
  let inBuff = open_in "test.json" in
  let lexbuf = Lexing.from_channel inBuff in
  
  Lexer.read lexbuf 