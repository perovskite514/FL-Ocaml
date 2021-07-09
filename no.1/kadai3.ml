let rec fix f x = f (fix f) x;;

let sum f n =
    if n = 0 then 0
    else f (n - 1) + n;;
let sum_to n = fix sum n;;

let prime f n k = 
    match k with 
    |1 -> true
    |m -> if (n mod m) = 0 then false
            else f n (k-1);;
let is_prime n =
    if n = 1 then false
    else fix prime n (n - 1);;

let gcd1 f a b =
    if b = 0 then a
    else f b (a mod b);;
let gcd a b = fix gcd1 a b;;


