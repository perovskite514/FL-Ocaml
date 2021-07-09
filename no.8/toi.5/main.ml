open Syntax
open Eval
open Typing
open Typing1

let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (ty, tyenv1) = infer_cmd tyenv cmd in
  let (id, newenv, v) = eval_command env cmd in
  (Printf.printf "%s : " id;
   print_type ty;
   print_string " = ";
   print_value v;
   print_newline ();
   read_eval_print newenv tyenv1)

let initial_env =
  extend "i" (VInt 1)
	 (extend "v" (VInt 5)
		 (extend "x" (VInt 10)
			 empty_env))

let initial_tyenv = 
  ((make_tyenv "i" (make_type_schema [] TyInt)) @
	 (make_tyenv "v" (make_type_schema [] TyInt)) @
		 (make_tyenv "x" (make_type_schema [] TyInt)) @
			 empty_env)

let _ = read_eval_print initial_env initial_tyenv
