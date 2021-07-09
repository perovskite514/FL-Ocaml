let curry f x y = f (x, y);;
let uncurry f (x, y) = f x y;;
let f = let count = ref (35) in fun x y -> count;;
let h = fun f -> f 0;;

