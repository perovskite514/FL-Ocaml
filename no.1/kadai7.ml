(*再帰的にリストの要素を一個ずつ列挙していく＋ filterを使って残りの要素のリストとpermする*)
let f x y = if x = y then false else true;;
let rec filter lst x = 
    match lst with 
    | [] -> []
    | fi :: res ->
        if f fi x then fi :: filter res x
                  else filter res x;;

let cons x y = x :: y;;
let rec map f x xs =
    match xs with
    | []  -> []
    | y :: ys -> f x y :: map f x ys;;
let rec perm2 lst1 lst2 =
    match (lst1, lst2) with
    | (_, []) -> [[]]
    | ([], _) -> []
    | (fi::res, _) -> map cons fi (perm2 (filter lst2 fi) (filter lst2 fi)) @ perm2 res lst2;; 
let perm lst = perm2 lst lst;;
