open Syntax
open Eval

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  try
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (id, newenv, v) = eval_command env cmd in
  (Printf.printf "%s = " id;
   print_value v;
   print_newline ();
   read_eval_print newenv)
  with
  | EvalErr -> print_string "Eval Error"; print_newline(); read_eval_print env 
  | Division_by_zero -> print_string "Division_by_zero"; print_newline(); read_eval_print env 
  | _ -> print_string "Unknown Token"; print_newline(); read_eval_print env

let initial_env =
  extend "i" (VInt 1)
	 (extend "v" (VInt 5)
		 (extend "x" (VInt 10)
			 empty_env))
    
let _ = read_eval_print initial_env
