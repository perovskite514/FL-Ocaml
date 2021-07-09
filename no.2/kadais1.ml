(*let fix f = (fun x -> f (x x)) (fun x -> f (x x));;*)
(*let fix f x = f (x x) f (x x);;*)
(*let fix f = (fun x -> f (fun y -> x x y)) (fun x -> f (fun y -> x x y));;*)
type 'a recc = 
        In of ('a recc -> 'a);; 
let out (In x) = x;; (*'a recc -> 'a recc -> 'a = <fun>*)
let fix f = (fun x a -> f (out x x) a) (In (fun x a -> f (out x x) a));;
let sum f n =
    if n = 0 then 0
    else f (n - 1) + n;;
let sum_to n = fix sum n;;