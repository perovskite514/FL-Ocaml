open Syntax

exception Unbound

type env = (name * dval ref) list

let empty_env = []
let extend x v env = (x, v) :: env

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

exception EvalErr

let rec eval_expr env e =
  match e with
  | EConstInt i ->
    VInt i
  | EConstBool b ->
    VBool b
  | EVar x ->
    (try
      let v = lookup x env in
       match !v with 
       | DVal v1 -> v1
       | DThunk (e, oenv) -> let v1 = eval_expr oenv e in v := (DVal v1) ; 
       (*print_name x; print_string " "; print_value v1; print_newline ();*) v1
     with
     | Unbound -> raise EvalErr)
  | EAdd (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
      | VInt i1, VInt i2 -> VInt (i1 + i2)
      | _ -> raise EvalErr)
  | ESub (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 - i2)
     | _ -> raise EvalErr)
  | EMul (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 * i2)
     | _ -> raise EvalErr)
  | EDiv (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> if i2 = 0 then raise Division_by_zero else VInt (i1 / i2)
     | _ -> raise EvalErr)
  | EEq (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 = i2)
     | VBool b1, VBool b2 -> VBool (b1 = b2)
     | _ -> raise EvalErr)
  | ELt (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 < i2)
     | VBool b1, VBool b2 -> VBool (b1 < b2)
     | _ -> raise EvalErr)
  | EIf (e1,e2,e3) ->
    let v1 = eval_expr env e1 in
    (match v1 with
     | VBool b ->
       if b then eval_expr env e2 else eval_expr env e3
     | _ -> raise EvalErr)
  | ELet (e1,e2,e3) -> 
    let v2 = ref (DThunk(e2,env)) in eval_expr (extend e1 v2 env) e3
  | EFun (x,e1) -> VFun (x,e1,env)
  | EApp (e1,e2) ->
      let v1 = eval_expr env e1 in
      let r = ref (DThunk(e2,env)) in
      (match v1 with 
        | VFun (x,e,oenv) -> eval_expr (extend x r oenv) e
        | VRecFun (f,x,e,oenv) -> let thunk = ref (DThunk(ELetFun(f,x,e), oenv)) in eval_expr ([(x, r)] @ [(f, thunk)] @ oenv) e
        | _ -> raise EvalErr)
  | ELetRec (f,x,e1,e2) -> 
      let thunk = ref (DThunk(ELetFun(f,x,e1), env)) in eval_expr ((extend f thunk) env) e2
  | ELetFun (f,x,e) -> VRecFun (f,x,e,env)
  
let rec eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (n, e) -> let v = ref (DThunk(e, env)) in (n, extend n v env, VNone)
  | CRecDecl (f, x, e) -> let thunk = ref (DThunk(ELetFun(f,x,e), env)) in (f, ([(f, thunk)] @ env), VRecFun (f, x, e, env))