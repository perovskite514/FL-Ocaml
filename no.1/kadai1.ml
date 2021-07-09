let rec sum_to n = 
    if n = 0 then 0
    else sum_to (n - 1) + n;;

let rec prime n k = 
    match k with 
    |1 -> true
    |m -> if (n mod m) = 0 then false
            else prime n (k-1);;
let is_prime n =
    if n = 1 then false
    else prime n (n - 1);;

let rec gcd a b =
    if b = 0 then a
    else gcd b (a mod b);;


