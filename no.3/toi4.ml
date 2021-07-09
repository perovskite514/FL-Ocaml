type order = LT | EQ | GT

module type ORDERED_TYPE = 
sig 
  type t
  val compare : t -> t -> order 
end

module type MAP = 
  functor (T : ORDERED_TYPE) -> 
    sig 
      type t 
      val empty : (T.t * 'a) list
      val add    : T.t -> 'a -> (T.t * 'a) list -> (T.t * 'a) list
      val remove : T.t -> (T.t * 'a) list -> (T.t * 'a) list
      val myLookup : T.t -> (T.t * 'a) list -> 'a
    end 

module Map : MAP = 
  functor (T : ORDERED_TYPE) -> struct 

    type t = T.t list
    let empty = [] 

    let rec add key a lst = match lst with
        | [] -> [(key, a)] 
        | (x, y) :: res -> (match T.compare key x with
                            | EQ -> (key, a) :: res
                            | _  -> (x, y) :: (add key a res))

    let rec remove key lst = match lst with
        | [] -> []
        | (x, y) :: res -> (match T.compare key x with
                            | EQ -> res
                            | _  -> (x, y) :: (remove key res))
    
    let rec myLookup key lst =
        match lst with
        | [] -> raise Not_found
        | ((x, y) :: res) -> (match T.compare key x with
                            | EQ -> y
                            | _  -> myLookup key res)

end

module OrderedInt =
struct
type t = int
let compare x y = if x < y then LT
                  else if x > y then GT
                  else EQ
end

module IntMap = Map (OrderedInt)

let s = IntMap.add 5 1 IntMap.empty;;
let s2 = IntMap.add 6 2 s;;
let s3 = IntMap.add 7 3 s2;;
let i = IntMap.myLookup 5 s3;;
let s4 = IntMap.add 5 4 s3;;
let i2 = IntMap.myLookup 5 s4;;
let s5 = IntMap.remove 5 s4;;
let i4 = IntMap.myLookup 5 s5;;

