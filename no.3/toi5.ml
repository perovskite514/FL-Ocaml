type intint = Int of int | Inf
module type SEMIRING = 
sig
type t
val add : t -> t -> t
val mul : t -> t -> t
val unit : t
val zero : t
end

module Matrix = 
  functor (T : SEMIRING) -> struct 

  type t = T.t list list

  exception Error
  exception SizeError

  let rec versize lst = match lst with
                      | [] -> 0
                      | a :: res -> (1 + versize res)
  
  let components lst = match lst with 
                     | [] -> []
                     | (a :: res) -> a;;

  let rec horsize lst = match lst with
                      | [] -> 0
                      | a :: res -> (1 + horsize res)

  let rec plus lst1 lst2 = match (lst1, lst2) with
                         | ([], []) -> []
                         | (a :: res1, b :: res2) -> (T.add a b) :: (plus res1 res2)
                         | (_, _) -> raise Error
                        
  let rec times lst1 lst2 acm = match (lst1, lst2) with
                         | ([], []) -> acm
                         | (_, []) -> acm
                         | (a :: res1, b :: res2) -> (times res1 res2 (T.add acm (T.mul a b)))
                         | (_, _) -> raise Error

  let rec transpose lst = match lst with 
                         | [] -> []
                         | [l] -> List.map (fun x -> [x]) l
                         | v :: l -> List.map2 (fun x y -> x::y) v (transpose l)

  let rec add v1 v2 = if ((versize v1 != versize v2) || ((horsize (components v1)) != (horsize (components v2)))) then raise SizeError
                      else match (v1, v2) with
                    | (ls1 :: res1, ls2 :: res2) -> (plus ls1 ls2) :: (add res1 res2)
                    | ([], []) -> []
                    | (_, _) -> raise Error
  
  let rec mul1 v1 v2 = match v2 with
                     | [] -> []
                     | ls :: res -> (times v1 ls T.zero) :: (mul1 v1 res)

  let rec mul2 v1 v2 = match v1 with
                     | ls1 :: res1 -> (mul1 ls1 (transpose v2)) :: (mul2 res1 v2)   
                     | [] -> []
  
  let mul v1 v2 = if ((horsize (components v1)) != (versize v2)) then raise SizeError 
                  else mul2 v1 v2

  end

module Bool =
struct
type t = bool
let add x y = x || y
let mul x y = x && y
let unit = true
let zero = false 
end

module Int =
struct
type t = intint
let add x y = match (x, y) with
            | (Inf, Inf) -> Inf
            | (Int a, Inf) -> Int a
            | (Inf, Int b) -> Int b
            | (Int a, Int b) -> if a < b then Int a else Int b
let mul x y = match (x, y) with
            | (Inf, Inf) -> Inf
            | (Int a, Inf) -> Inf
            | (Inf, Int b) -> Inf
            | (Int a, Int b) -> Int (a + b)
let unit = Int 0
let zero = Inf 
end

module BoolMatrix = Matrix (Bool)
module IntMatrix = Matrix (Int)

(** 
let v1 = [[true; true]; [false; false]];;
let v2 = [[true; false]; [true; true]];;
let v3 = [[true; true; false]; [false; false; true]];;
let s = BoolMatrix.add v1 v2;;
let s2 = BoolMatrix.mul v1 v3;;
let s3 = BoolMatrix.add v1 v3;;

let v4 = [[Int 1; Int (-1)]; [Int 2; Int (-2)]];;
let v5 = [[Int 10; Int (-10)]; [Int 5; Int (-5)]];;
let v6 = [[Int 1; Int 5; Int 10]; [Int (-1); Int (-5); Int (-10)]];;
let v7 = [[Int 1; Int 5]; [Int (-1); Int (-5)]; [Int (3); Int (-10)]];;
let s4 = IntMatrix.add v4 v5;;
let s5 = IntMatrix.mul v4 v6;;
let s6 = IntMatrix.mul v4 v7;;
*)

let v8 = [[Int 0; Int 10; Int 1; Int 15; Int 50]; [Int 10; Int 0; Inf; Inf; Int 14]; [Int 1; Inf; Int 0; Int 5; Inf]; [Int 15; Inf; Int 5; Int 0; Inf]; [Int 50; Int 14; Inf; Inf; Int 0]]
let v9 = IntMatrix.mul v8 v8;;
let v10 = IntMatrix.mul v9 v8;;
let v11 = IntMatrix.mul v10 v8;;