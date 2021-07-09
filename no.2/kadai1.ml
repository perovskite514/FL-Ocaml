type nat = Z | S of nat;;
let zero = Z;;
let one = S Z;;
let two = S (S Z);;
let three = S (S (S Z));;
let rec add n m = match n with 
    | Z -> m 
    | S (i) ->  S (add i m);;
let rec sub n m = match (n , m) with
    | (Z, _) -> Z
    | (_, Z) -> n
    | (S (i), S (j)) -> sub i j;;
let rec mut n m = match n with
    | Z -> Z
    | S (i) -> add m (mut i m);;
let rec pow n m = match m with
    | Z -> S Z
    | S (i) -> mut n (pow n i);;
let rec ni m acm = match m with 
    | Z -> acm
    | S (i) -> ni i (acm + 1);;
let n2i n = ni n 0;;
let rec repeat f n x = 
        if n = 0 then x
        else f(repeat f (n-1) x);;
let rec i2n n =
        if n = 0 then Z
        else S (i2n (n - 1));;