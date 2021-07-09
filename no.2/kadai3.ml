type 'a tree =
    | Leaf
    | Node of 'a * 'a tree * 'a tree;;

let tree = Node(1, Node(2, Node(3, Leaf, Leaf), Node(4, Leaf, Leaf)), Node(5, Node(6, Leaf, Leaf), Node(7, Leaf, Leaf)));;

(*リストを逆順にする*)    
let rec rev lst acm =
    match lst with
    | [] -> acm
    | fi :: res -> rev res (fi :: acm);;
let reverse lst = rev lst [];;

(*木の深さを求める*)
let rec depth x = match x with
    | Leaf -> 0
    | Node (_, l, r) -> 1 + if (depth l) < (depth r) then (depth r) else (depth l);;

(*深さkの値を取ってくる*)
let first (x, y) = x;;
let second (x, y) = y;;
let rec filter lst d = 
    match lst with 
    | [] -> []
    | fi :: res ->
        if second fi = d then first fi :: filter res d 
                else filter res d;;

let rec bfs1 x y = match y with
    | -1 -> []
    | yy -> (filter x yy) @ (bfs1 x (y - 1));;
let rec dfs x y = match x with
    | Leaf -> []
    | Node (a, b, c) -> (a , y) :: (dfs c (y + 1)) @ (dfs b (y + 1));;
let bfs x = reverse (bfs1 (dfs x 0) (depth x));;  