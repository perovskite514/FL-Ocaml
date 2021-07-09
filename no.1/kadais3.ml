let f y = y + 1;;
let zero f x = x;;
let succ n f x = f (n f x);;
let to_int n = n (fun k -> k + 1) 0;;
let rec repeat f n x = 
        if n < 1 then x
        else f(repeat f (n - 1) x);;
let add n m f x = m f (n f x);;
let mut n m f x = repeat f ((to_int n) * (to_int m)) x;;
let sub n m f x = repeat f ((to_int n) - (to_int m)) x;;
