let rec fold_right f lst e =
    match lst with
    | [] -> e
    | fi :: res -> f fi (fold_right f res e);;

let rec fold_left f e lst =
    match lst with
    | [] -> e
    | fi :: res -> (fold_left f (f fi e) res);;

let rec rev lst acm =
    match lst with
    | [] -> acm
    | fi :: res -> rev res (fi :: acm);;
let reverse lst = rev lst [];;
let f fi res = fi :: res;;

(*append_right, filter_rightがfold_rightを使っています.
  append_left, filter_leftはfold_leftを使っています.*)

let rec append_right lst1 lst2 = 
    fold_right f lst1 lst2;;

let rec append_left lst1 lst2 =
    fold_left f lst2 (reverse lst1);;

(*eが返り値なのでeに選択した値を累積していく
判定関数部分を補助関数を用いて工夫する*)

let ff judge fi res = if judge fi then fi :: res else res;;
let rec filter_right lst judge = 
    fold_right (fun fi res -> if(judge fi) then fi :: res else res) lst [];;
let rec filter_left lst judge =
    fold_left (fun fi res -> if(judge fi) then fi :: res else res) [] (reverse lst);; 




