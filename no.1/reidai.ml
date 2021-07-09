let circle r = r *. r *. 3.14159;;
circle 10.0;;
circle 15.0;;

let rec sigma f n = 
        if n = 0 then 0
        else f n + sigma f (n - 1);;
let f x = x * x + x;;
let n = 10;;
sigma f n;;

let rec map f xs =
    match xs with
    | []  -> []
    | y :: ys -> f y :: map f ys;;
map (fun x -> x * x) [1; 2; 3];;
