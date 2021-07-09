let rec rev lst acm =
    match lst with
    | [] -> acm
    | fi :: res -> rev res (fi :: acm);;
let reverse lst = rev lst [];;

let first (x, y) = x;;
let second (x, y) = y;;
let rec fold_right f lst e =
    match lst with
    | [] -> e
    | fi :: res -> f fi (fold_right f res e);;
let f x y = 
    match y with 
    | (_, []) -> ([], [])
    | (lst, fi :: res) -> (fi :: lst, res)
let reverse2 lst = 
    first (fold_right f lst ([], lst));;
