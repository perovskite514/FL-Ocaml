module type EQ = sig
type ('a, 'b) equal
val refl : ('a, 'a) equal
val symm : ('a, 'b) equal -> ('b, 'a) equal
val trans :
('a, 'b) equal -> ('b, 'c) equal -> ('a, 'c) equal
val apply : ('a, 'b) equal -> 'a -> 'b
end

module Eq = struct
type ('a, 'b) equal = T : ('a, 'a) equal
let refl = T
let symm (type a b) (T : (a, b) equal) : (b, a) equal = T
let trans (type a b c) (T : (a, b) equal) (T : (b, c) equal) : (a, c) equal = T
let apply  (type a b) (T : (a, b) equal) (a : a) : b = a
end

module Eq1 : EQ = Eq
let a = Eq.refl;
