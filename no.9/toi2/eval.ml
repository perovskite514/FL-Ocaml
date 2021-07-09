open Syntax

exception Unbound

type env = (name * value) list

let empty_env = []
let extend x v env = (x, v) :: env

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

exception EvalErr

let rec find_match pat value = (match pat with
         | PInt i -> (match value with
                        | VInt j -> if i = j then Some [] else None
                        | _ -> None)
         | PBool i -> (match value with
                        | VBool j -> if i = j then Some [] else None
                        | _ -> None)
         | PVar n -> Some [(n, value)]
         | PPair (i, j) -> (match value with
                              | VPair (ii, jj) -> (match (find_match i ii) , (find_match j jj) with
                                                   | Some i, Some j -> Some (i @ j)
                                                   | _, _ -> None)
                              | _ -> None)
         | PNil -> (match value with
                        | VNil -> Some []
                        | _ -> None)
         | PCons (i, j) -> (match value with
                              | VCons (ii, jj) -> (match (find_match i ii) , (find_match j jj) with
                                                   | Some i, Some j -> Some (i @ j)
                                                   | _ -> None)
                              | _ -> None))

let rec pat_match value lst i = (match lst with
                  | [] -> raise EvalErr
                  | (p, e) :: res -> (match find_match p value with
                                  | None -> pat_match value res (i + 1)
                                  | _ -> i))

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
    let v2 = eval_expr env e2 in eval_expr (extend e1 v2 env) e3
  | EFun (x,e1) -> VFun (x,e1,env)
  | EApp (e1,e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match v1 with 
        | VFun (x,e3,oenv) -> eval_expr (extend x v2 oenv) e3
        | VRecFun (f,x,e3,oenv) -> let env' = extend x v2 (extend f (VRecFun(f,x,e3,oenv)) oenv) in eval_expr env' e3
        | _ -> raise EvalErr)
  | ELetRec (f,x,e1,e2) -> 
      let env' = extend f (VRecFun(f,x,e1,env)) env in eval_expr env' e2
  | EPair (e1, e2) -> let v1 = eval_expr env e1 in
                      let v2 = eval_expr env e2 in
                      VPair (v1, v2)
  | ENil -> VNil
  | ECons (e1, e2) -> let v1 = eval_expr env e1 in
                      let v2 = eval_expr env e2 in
                      VCons (v1, v2)
  | EMatch (e1, lst) -> let v1 = eval_expr env e1 in
                        let i = pat_match v1 lst 0 in
                        let (p, e2) = (List.nth lst i) in
                        match find_match p v1 with
                        | None -> raise EvalErr
                        | Some lst -> eval_expr (env @ lst) e2
                        
let rec eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (n, e) -> let e1 = eval_expr env e in (n, extend n e1 env, e1)
  | CRecDecl (f, x, e) -> (f, extend f (VRecFun (f, x, e, env)) env, VRecFun (f, x, e, env))