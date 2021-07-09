let rec rev lst acm =
    match lst with
    | [] -> acm
    | fi :: res -> rev res (fi :: acm);;
let reverse lst = rev lst [];;

let f fi res = fi :: res;;

let rec fold_right f lst e =
    match lst with
    | [] -> e
    | fi :: res -> f fi (fold_right f res e);;

let rec fold_left f e lst =
    match lst with
    | [] -> e
    | fi :: res -> (fold_left f (f fi e) res);;

(*fold_right2とfold_left2が課題要件の関数です.*)

let fold_right2 f lst e = fold_left f e (reverse lst);;
let fold_left2 f e lst = fold_right f (reverse lst) e;;

