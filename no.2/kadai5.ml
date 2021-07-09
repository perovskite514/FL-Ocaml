type value = VInt of int | VBool of bool;;
exception Eval_error;;
type expr =
    | EConstInt of int
    | EAdd of expr * expr
    | ESub of expr * expr
    | EMul of expr * expr
    | EDiv of expr * expr
    | EConstBool of bool
    | EEqual of expr * expr
    | EComp of expr * expr
    | EIf of expr * expr * expr

let test1 = EIf (EAdd (EConstInt 1, EConstInt 2),
           EComp (EConstInt 3, EConstInt 4),
           EEqual (EConstInt 5, EConstInt 4));;
let test2 = EIf (EComp (EConstInt 1, EConstInt 2),
           EComp (EConstInt 3, EConstInt 4),
           EEqual (EConstInt 5, EConstInt 4));;
let test3 = EAdd (ESub (EConstInt 1, EConstInt 3) , EMul (EConstInt 4, EDiv (EConstInt 4, EConstInt 2)));;
let test4 = EAdd (ESub (EConstBool true, EConstInt 3) , EMul (EConstInt 4, EDiv (EConstInt 4, EConstInt 2)));;
let rec eval expr = match expr with
    | EConstInt (n) -> VInt (n)
    | EAdd (x , y) -> (match (eval x , eval y) with
                           | (_ , VBool (i)) -> raise Eval_error
                           | (VBool (i) , _) -> raise Eval_error
                           | (VInt (i) , VInt (j)) -> VInt (i + j))
    | ESub (x , y) -> (match (eval x , eval y) with
                           | (_ , VBool (i)) -> raise Eval_error
                           | (VBool (i) , _) -> raise Eval_error
                           | (VInt (i) , VInt (j)) -> VInt (i - j))
    | EMul (x , y) -> (match (eval x , eval y) with
                           | (_ , VBool (i)) -> raise Eval_error
                           | (VBool (i) , _) -> raise Eval_error
                           | (VInt (i) , VInt (j)) -> VInt (i * j))
    | EDiv (x , y) -> (match (eval x , eval y) with
                           | (_ , VBool (i)) -> raise Eval_error
                           | (VBool (i) , _) -> raise Eval_error
                           | (_ , VInt (0)) -> raise Eval_error
                           | (VInt (i) , VInt (j)) -> VInt (i / j))
    | EConstBool (n) -> VBool (n)
    | EEqual (x , y) -> (match (eval x , eval y) with
                           | (VBool (i) , VBool (j)) -> VBool (i = j)
                           | (VInt (i) , VInt (j)) -> VBool (i = j)
                           | (_ , _) -> raise Eval_error)
    | EComp (x , y) -> (match (eval x , eval y) with
                           | (VBool (i) , VBool (j)) -> VBool (i < j)
                           | (VInt (i) , VInt (j)) -> VBool (i < j)
                           | (_ , _) -> raise Eval_error)
    | EIf (x, y, z) -> (match (eval x , eval y, eval z) with
                           | (VBool (i) , _ , _) -> if i then eval y else eval z
                           | (_ , _, _ ) -> raise Eval_error);;
