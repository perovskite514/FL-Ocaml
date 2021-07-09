let twice f x = f(f x);;
let rec repeat f n x = 
        if n = 0 then x
        else f(repeat f (n-1) x);;
