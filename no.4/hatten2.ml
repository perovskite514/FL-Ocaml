let return x = [x]
let p a = if a = [] then true else false
let (>>=) x f = if List.exists p (List.map f x) then [] else List.concat (List.map f x)

let x = [1; 10; 100];;
let f = fun i -> [i - 1; i + 1];;
let guard b = if b then return () else []
let find =
    [1; 2; 3] >>= (fun x ->
    [4; 5; 6] >>= (fun y ->
    (guard (x + y > 7)) >>= (fun _ ->
    return (x, y))));;
x >>= f;;