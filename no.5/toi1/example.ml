open Syntax

let main s =
  try 
    let lexbuf = Lexing.from_string s in 
    let result = Parser.toplevel Lexer.main lexbuf in
    print_command result; print_newline ()
  with 
    | Parsing.Parse_error -> 
      print_endline "Parse Error!"
      
;;
let _ = main Sys.argv.(1);;

    
    
  
  
