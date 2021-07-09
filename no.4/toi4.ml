type 'a m = (int * int) list -> ('a * (int * int) list)
type 'a option = Some of 'a | None

let first (x, y) = x;;
let second (x, y) = y;;

(** (>>=) : 'a m -> ('a -> 'b m) -> 'b m *)
let (>>=) (x : 'a m) (f : 'a -> 'b m) = fun init ->
                                          let (r, s) = x init in                                          
                                          f r s;;
(** return : 'a -> 'a m *)
let return x = (fun init -> (x, init));;

let rec myLookup key xs = match xs with
        | [] -> None 
        | ((k, v) :: rest) -> if key = k then Some v
                                       else myLookup key rest;;

(** memo : (int -> int m) -> int -> int m *)
let memo (f : int -> int m) n =
  (fun env -> match myLookup n env with
            | None -> (match ((f n) env) with | (x, y) -> (x, (n, x) :: y))
            | Some v -> (v, env));;

(** runMemo : 'a m -> 'a *)
let runMemo (x : 'a m) = first(x []);;

let rec fib n =
  if n <= 1 then
    return n
  else
    (memo fib (n-2)) >>= (fun r1 ->
    (memo fib (n-1)) >>= (fun r2 ->
      return (r1 + r2)))

let _ =
  if runMemo (fib 80) = 23416728348467685 && runMemo (fib 10) = 55 then
    print_string "ok\n"
  else
    print_string "wrong\n"