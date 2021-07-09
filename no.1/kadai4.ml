let rec fold_right f lst e =
    match lst with
    | [] -> e
    | fi :: res -> f fi (fold_right f res e);;

let rec fold_left f e lst =
    match lst with
    | [] -> e
    | fi :: res -> (fold_left f (f fi e) res);;