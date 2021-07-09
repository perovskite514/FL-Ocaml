open Typing1
open Syntax

exception Unbound
exception TypeErr

type type_schema = tyvar list * ty
type tyenv = (name * type_schema) list

let empty_tyenv = []
let infer_extend x v tyenv = (x, v) :: tyenv
let second_list lst = List.map (fun (t1, t2) -> t2) lst  
let rec lookup x tyenv =
  try List.assoc x tyenv with Not_found -> raise Unbound

(*make_はそのものを作成する関数*)
let make_type_schema lst ty : type_schema = let tys = (lst, ty) in tys
let make_tyenv n tys : tyenv = let tye = (n, tys) in [tye]
let rec make_subst lst = match lst with
        | [] -> []
        | tyvar :: res -> (tyvar, (TyVar (new_tyvar ()))) :: make_subst res

(*substの中からty_schemaに含まれているものを除く*)
let rec set_sigma sigma lst = match sigma with
        | [] -> []
        | (tyvar, ty) :: res -> if List.exists (fun x -> x == tyvar) lst then set_sigma res lst else (tyvar, ty) :: set_sigma res lst 

let rec delta2 sigma ts = let (lst, ty) = ts in
               let sigma2 = set_sigma sigma lst in
               (lst, ty_subst sigma2 ty)

let rec delta1 sigma tyenv = match tyenv with
        | [] -> []
        | (n, ts) :: res -> (n, delta2 sigma ts) :: (delta1 sigma res)

let rec ty_list subst lst = match lst with 
                            | [] -> []
                            | (ty) :: lst -> (ty_subst subst ty) :: ty_list subst lst 

let rec s1_list ty =
        match ty with
            | TyFun (t1, t2) -> (s1_list t1) @ (s1_list t2)
            | TyVar (i) -> [i]
            | _ -> []

let rec s2_list lst = 
        match lst with
            | [] -> []
            | t1 :: res -> (s1_list t1) @ (s2_list res)

let rec set_difference lst1 lst2 =
    match lst1 with
        | [] -> []
        | t1 :: res -> if List.exists (fun x -> t1 == x) lst2 then set_difference res lst2 else t1 :: set_difference res lst2

let rec infer_expr tyenv e =
    match e with
    | EConstInt i -> (TyInt, [])
    | EConstBool b -> (TyBool, [])
    | EVar x -> 
     (try
        let tys = lookup x tyenv in
        let (lst, ty) = tys in
        ((ty_subst (make_subst lst) ty), [])
        with
        | Unbound -> raise TypeErr)
    | EAdd (e1, e2) -> 
        let (ty1, constraints1) = infer_expr tyenv e1 in
        let (ty2, constraints2) = infer_expr tyenv e2 in
            (TyInt,  [(TyInt, ty1); (TyInt, ty2)] @ constraints1 @ constraints2)
    | ESub (e1, e2) -> 
        let (ty1, constraints1) = infer_expr tyenv e1 in
        let (ty2, constraints2) = infer_expr tyenv e2 in
            (TyInt,  [(TyInt, ty1); (TyInt, ty2)] @ constraints1 @ constraints2)
    | EMul (e1, e2) -> 
        let (ty1, constraints1) = infer_expr tyenv e1 in
        let (ty2, constraints2) = infer_expr tyenv e2 in
            (TyInt,  [(TyInt, ty1); (TyInt, ty2)] @ constraints1 @ constraints2)
    | EDiv (e1, e2) -> 
        let (ty1, constraints1) = infer_expr tyenv e1 in
        let (ty2, constraints2) = infer_expr tyenv e2 in
            (TyInt,  [(TyInt, ty1); (TyInt, ty2)] @ constraints1 @ constraints2)
    | EEq (e1, e2) -> 
        let (ty1, constraints1) = infer_expr tyenv e1 in
        let (ty2, constraints2) = infer_expr tyenv e2 in
            (TyBool,  [(ty1, ty2)] @ constraints1 @ constraints2)
    | ELt (e1, e2) -> 
        let (ty1, constraints1) = infer_expr tyenv e1 in
        let (ty2, constraints2) = infer_expr tyenv e2 in
            (TyBool,  [(TyInt, ty1); (TyInt, ty2); (ty1, ty2)] @ constraints1 @ constraints2)
    | EIf (e1,e2,e3) ->
        let (ty1, constraints1) = infer_expr tyenv e1 in
        let (ty2, constraints2) = infer_expr tyenv e2 in
        let (ty3, constraints3) = infer_expr tyenv e3 in
        (ty2, [(TyBool, ty1); (ty2, ty3)] @ constraints1 @ constraints2 @ constraints3)
    | ELet (x,e2,e3) ->
        let (ty1, constraints1) = infer_expr tyenv e2 in
        let sigma = unify constraints1 in
        let s1 = ty_subst sigma ty1 in
        let s1_set = s1_list s1 in 
        let delta = delta1 sigma tyenv in
        let delta_set = s2_list (second_list (second_list delta)) in
        let p = set_difference s1_set delta_set in
        let newtyenv = make_tyenv x (make_type_schema p s1) in
        let (ty2, constraints2) = infer_expr (newtyenv @ delta) e3 in
        (ty2, constraints1 @ constraints2)
    | EFun (x,e1) -> 
        let a = new_tyvar () in
        let (ty, constraints) = infer_expr ((make_tyenv x (make_type_schema [] (TyVar a))) @ tyenv) e1 in
        (TyFun ((TyVar a), ty), constraints)
    | EApp (e1,e2) -> 
        let (ty1, constraints1) = infer_expr tyenv e1 in
        let (ty2, constraints2) = infer_expr tyenv e2 in
        let a = new_tyvar () in
        (TyVar a, [(ty1, TyFun (ty2, TyVar a))] @ constraints1 @ constraints2)
    | ELetRec (f,x,e1,e2) -> 
        let a = new_tyvar () in
        let b = new_tyvar () in
        let (ty1, constraints1) = infer_expr (tyenv @ 
        (make_tyenv f (make_type_schema [] (TyFun ((TyVar a) , (TyVar b))))) @ (make_tyenv x (make_type_schema [] (TyVar a))))  e1 in
        let sigma = unify constraints1 in 
        let s1 = ty_subst sigma ty1 in
        let s1_set = s1_list s1 in 
        let delta = delta1 sigma tyenv in
        let delta_set = s2_list (second_list (second_list delta)) in
        let p = set_difference s1_set delta_set in
        let s2 = ty_subst sigma (TyVar a) in
        let (ty2, constraints2) = infer_expr (delta @ (make_tyenv f (make_type_schema p (TyFun (s2, s1))))) e2 in
        (ty2, constraints1 @ constraints2)

let infer_cmd tyenv c = 
    match c with
    | CExp e -> let (ty, constraints) = infer_expr tyenv e in 
                    (ty_subst (unify constraints) ty, tyenv)
    | CDecl (n, e) -> let (ty, constraints) = infer_expr tyenv e in 
                    (ty_subst (unify constraints) ty, (make_tyenv n (make_type_schema [] ty)) @ tyenv)
    | CRecDecl (f, x, e) -> 
        let a = new_tyvar () in
        let b = new_tyvar () in
        let (ty, constraints) = infer_expr (tyenv @  
        (make_tyenv x (make_type_schema [] (TyVar a))) @ (make_tyenv f (make_type_schema [] (TyFun ((TyVar a) , (TyVar b)))))) e in
        let sigma = unify constraints in 
        let s1 = ty_subst sigma ty in
        let s1_set = s1_list s1 in 
        let delta = delta1 sigma tyenv in
        let delta_set = s2_list (second_list (second_list delta)) in
        let p = set_difference s1_set delta_set in
        let s2 = ty_subst sigma (TyVar a) in
        (ty_subst (unify (infer_extend ty (TyVar b) constraints)) (TyFun ((TyVar a) , (TyVar b))), (delta @ (make_tyenv f (make_type_schema p (TyFun (s2, s1))))))