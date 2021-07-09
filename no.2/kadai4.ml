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