type order = LT | EQ | GT
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

module type ORDERED_TYPE = 
sig 
  type t
  val compare : t -> t -> order 
end

module type MULTISET2 = 
  functor (T : ORDERED_TYPE) -> 
    sig 
      type t 
      val empty : t 
      val add    : T.t -> t -> t 
      val remove : T.t -> t -> t 
      val count  : T.t -> t -> int 
    end 

module Multiset2 : MULTISET2 =
  functor (T : ORDERED_TYPE) -> struct 

    type t = T.t tree

    let rec min t =
        match t with
        | Leaf -> raise Not_found
        | Node (l, n, r) -> (match l with 
                    | Leaf -> n 
                    | Node (ll, nn, rr) -> min ll)

    let rec remove a tree = 
      match tree with 
	  | Leaf -> Leaf
      | Node (l, n, r) ->  (match T.compare a n with 
	               | LT -> Node ((remove n l), n, r)
	               | GT -> Node (l, n, (remove n r))
                   | EQ -> (match l, r with
                        | Leaf, Leaf -> Leaf
                        | Leaf, Node _ -> r
                        | Node _, Leaf -> l
                        | Node _, Node _ -> let m = (min r) in Node (l, n, remove m r)))
	
    let empty = Leaf

    let rec add a tree =
      match tree with
      | Leaf -> Node (Leaf, a, Leaf)
      | Node (l, n, r) -> (match T.compare a n with
	                | LT -> Node (add a l, n, r)
	                | EQ -> Node (l, n, add a r)
	                | GT -> Node (l, n, add a r))

    let rec count_sub a tree =
      match tree with 
	  | Leaf -> 0 
	  | Node (l, n, r)  -> (match T.compare a n with
	                    | LT -> ((count_sub a l) + (count_sub a r))
	                    | EQ -> ((count_sub a l) + (count_sub a r) + 1)
	                    | GT -> ((count_sub a l) + (count_sub a r)))

    let count a tree = count_sub a tree 

  end

module OrderedInt =
struct
type t = int
let compare x y = if x < y then LT
                  else if x > y then GT
                  else EQ
end

module IntMultiset = Multiset2 (OrderedInt)

let s = IntMultiset.add 5 IntMultiset.empty;;
let s2 = IntMultiset.add 5 s;;
let s3 = IntMultiset.add 2 s2;; 
let i = IntMultiset.count 5 s3;;
let i2 = IntMultiset.count 2 s3;;
let s4 = IntMultiset.remove 5 s3;;
let i3 = IntMultiset.count 5 s4;;
let s5 = IntMultiset.remove 5 s4;;
let i4 = IntMultiset.count 5 s5;;
let s6 = IntMultiset.remove 5 s5;;
let i5 = IntMultiset.count 5 s6;;