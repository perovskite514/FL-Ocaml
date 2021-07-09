type tyvar = int
	      
type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar

exception TyError
exception Unbound

type subst = (tyvar * ty) list
type constraints = (ty * ty) list

let new_tyvar = let counter = ref 0 in 
                let var () = let v = !counter in counter := v + 1; v in var

let print_type ty =
  let rec type_to_string ty =
  match ty with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1, t2) -> "(" ^ (type_to_string t1) ^ ") -> (" ^ (type_to_string t2) ^ ")"
  | TyVar i -> "a" ^ (string_of_int i)
  in print_string (type_to_string ty)

let rec lookup x tyenv =
  try List.assoc x tyenv with Not_found -> raise Unbound

let rec ty_subst subst ty =  
    let rec ty_subst2 subst2 ty2 = 
    match subst2 with
        | [] -> TyVar (ty2)
        | (ty3, t1) :: subst3 -> if ty3 = ty2 then t1 else ty_subst2 subst3 ty2
    in match ty with
        | TyInt -> TyInt
        | TyBool -> TyBool
        | TyFun (t1, t2) -> TyFun (ty_subst subst t1, ty_subst subst t2)
        | TyVar (i) -> ty_subst2 subst i

let rec compose sigma1 sigma2 : subst =
    let sigma3 = List.map (fun (t1, t2) -> (t1, ty_subst sigma1 t2)) sigma2 in 
    List.fold_left (fun t3 -> fun (t1, t2) -> try let _ = lookup t1 sigma2 in t3
		                                          with Unbound -> (t1, t2) :: t3) sigma3 sigma1

let rec occurs t1 t2 =
    if t1 = t2 then true else 
    match t2 with
      | TyFun(t3, t4) -> (occurs t1 t3) || (occurs t1 t4)
      | _ -> false

let unify cons =  
    let rec solve cons subs =
        match cons with
            | [] -> subs
            | (t1, t2) :: cons2 -> if t1 = t2 then solve cons2 subs else
        begin match (t1, t2) with
	    | (TyFun(t1, t2), TyFun(t3, t4)) -> solve ((t1, t3) :: (t2, t4) :: cons2) subs
	    | (TyVar i, _) -> if (occurs t1 t2) then raise Not_found
                          else let s = [(i, t2)] in 
                          solve (List.map (fun (t1, t2) -> (ty_subst s t1, ty_subst s t2)) cons2) 
                                                                (compose s subs)
      | (_, TyVar i) -> if (occurs t2 t1) then raise Not_found
                          else let s = [(i, t1)] in
                          solve (List.map (fun (t1, t2) -> (ty_subst s t1, ty_subst s t2)) cons2) 
                                                                (compose s subs)
	    | (_, _) -> raise Not_found
        end
  in solve cons []
