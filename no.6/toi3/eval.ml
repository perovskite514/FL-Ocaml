open Syntax

exception Unbound

type env = (name * value) list

let empty_env = []
let extend x v env = (x, v) :: env

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

let rec nlookup lst n = match lst with
        | (f, x, e) :: res -> if n = 1 then x else nlookup res (n - 1)
        | _ -> assert false

let rec nlookup1 lst n = match lst with
        | (f, x, e) :: res -> if n = 1 then e else nlookup1 res (n - 1)
        | _ -> assert false
  
exception EvalErr

let rec addenv constlst constenv n lst env = match lst with
    | [] -> env
    | (f, x, e) :: res -> addenv constlst constenv (n + 1) res (extend f (VRecFun(n, constlst, constenv)) env)

let rec eval_expr env e =
  match e with
  | EConstInt i ->
    VInt i
  | EConstBool b ->
    VBool b
  | EVar x ->
    (try
       lookup x env
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
    let v2 = eval_expr env e2 in
    (match v2 with
     | VInt _ -> eval_expr (extend e1 v2 env) e3
     | VBool _ -> eval_expr (extend e1 v2 env) e3
     | _ -> raise EvalErr)
  | EFun (x,e1) -> VFun (x,e1,env)
  | EApp (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match v1 with 
        | VFun (x,e3,oenv) -> eval_expr (extend x v2 oenv) e3
        | VRecFun (inx, lst, oenv) -> 
                  let env' = extend (nlookup lst inx) v2 (addenv lst oenv 1 lst oenv) in eval_expr env' (nlookup1 lst inx)
        | _ -> raise EvalErr)
  | ELetRec (lst, e2) -> eval_expr (addenv lst env 1 lst env) e2
  
let rec eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (n, e) -> let e1 = eval_expr env e in (n, extend n e1 env, e1)
  | CRecDecl decls -> ("-", addenv decls env 1 decls env, VPrint 1)