open Syntax
open Typing1

exception Unbound

type tyenv = (name * ty) list
let empty_tyenv = []
let infer_extend x v tyenv = (x, v) :: tyenv

let rec lookup x tyenv =
  try List.assoc x tyenv with Not_found -> raise Unbound

exception TypeErr

let rec infer_expr tyenv e = 
    match e with
    | EConstInt i -> (TyInt, [])
    | EConstBool b -> (TyBool, [])
    | EVar x -> 
     (try
        (lookup x tyenv, [])
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
        let (ty2, constraints2) = infer_expr tyenv e2 in
        let (ty3, constraints3) = infer_expr (infer_extend x ty2 tyenv) e3 in
        (ty3, constraints2 @ constraints3)
    | EFun (x,e1) -> 
        let a = new_tyvar () in
        let (ty, constraints) = infer_expr (infer_extend x (TyVar a) tyenv) e1 in
        (TyFun ((TyVar a), ty), constraints)
    | EApp (e1,e2) -> 
        let (ty1, constraints1) = infer_expr tyenv e1 in
        let (ty2, constraints2) = infer_expr tyenv e2 in
        let a = new_tyvar () in
        (TyVar a, [(ty1, TyFun (ty2, TyVar a))] @ constraints1 @ constraints2)
    | ELetRec (f,x,e1,e2) -> 
        let a = new_tyvar () in
        let b = new_tyvar () in
        let (ty1, constraints1) = infer_expr (infer_extend x (TyVar a) (infer_extend f (TyFun (TyVar a, TyVar b)) tyenv)) e1 in
        let (ty2, constraints2) = infer_expr (infer_extend f (TyFun (TyVar a, TyVar b)) tyenv) e2 in
        (ty2, [(ty1, TyVar b)] @ constraints1 @ constraints2)

let infer_cmd tyenv c = 
    match c with
    | CExp e -> let (ty, constraints) = infer_expr tyenv e in 
                    (ty_subst (unify constraints) ty, tyenv)
    | CDecl (n, e) -> let (ty, constraints) = infer_expr tyenv e in 
                    (ty_subst (unify constraints) ty, infer_extend n ty tyenv)
    | CRecDecl (f, x, e) -> 
        let a = new_tyvar () in
        let b = new_tyvar () in
        let (ty, constraints) = infer_expr (infer_extend x (TyVar a) (infer_extend f (TyFun (TyVar a, TyVar b)) tyenv)) e in
        (ty_subst (unify (infer_extend ty (TyVar b) constraints)) (TyFun (TyVar a, TyVar b)), (infer_extend x (TyVar a) (infer_extend f (TyFun (TyVar a, TyVar b)) tyenv)))