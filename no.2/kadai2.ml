type 'a tree =
    | Leaf
    | Node of 'a * 'a tree * 'a tree;;

let tree = Node(1, Node(2, Node(3, Leaf, Leaf), Node(4, Leaf, Leaf)), Node(5, Node(6, Leaf, Leaf), Node(7, Leaf, Leaf)));;

(*pre-order*)
let rec dfs_pre x = match x with
    | Leaf -> []
    | Node (a, b, c) -> a :: (dfs_pre b) @ (dfs_pre c);;

(*in-order*)
let rec dfs_in x = match x with
    | Leaf -> []
    | Node (a, b, c) -> (dfs_in b) @ (a :: (dfs_in c));;

(*post-order*)
let rec dfs_post x = match x with
    | Leaf -> []
    | Node (a, b, c) -> (dfs_post b) @ (dfs_post c) @ [a];;
