(* Definition of "the" list monad *)
type 'a m = 'a list

(** (>>=) : 'a m -> ('a -> 'b m) -> 'b m *)
let (>>=) (x : 'a m) (f : 'a -> 'b m) =
  List.concat (List.map f x)

(** return : 'a -> 'a m *)
let return (x : 'a) = [x]

(** guard : bool -> unit m *)
let guard (x : bool) =
  if x then return () else []

let sinamon b n s m nn = if 200 * b + 22 * n = 1000 * s + 100 * n + 10 * m + nn then true else false;;
let hukumen1 = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9;] >>= (fun b ->
            [0; 1; 2; 3; 4; 5; 6; 7; 8; 9;] >>= (fun n ->
            [0; 1; 2; 3; 4; 5; 6; 7; 8; 9;] >>= (fun s ->
            [0; 1; 2; 3; 4; 5; 6; 7; 8; 9;] >>= (fun m ->
            [0; 1; 2; 3; 4; 5; 6; 7; 8; 9;] >>= (fun nn ->
            (guard (sinamon b n s m nn)) >>= (fun _ ->
                return (b, n, s, m, nn)))))));;

let rec filter lst x = 
    match lst with 
    | [] -> []
    | fi :: res ->
        if fi != x then fi :: filter res x 
                  else filter res x;;
let kenzan s m lst = if 1000 * s + 1000 > 9000 * m  then lst else [];;
Sys.time();;
let money s e n d m o r y = if 1000 * s + 91 * e - 90 * n + d - 9000 * m - 900 * o + 10 * r - y = 0 then true else false 
let hukumen2 = [1; 2; 3; 4; 5; 6; 7; 8; 9;] >>= (fun s ->
            (filter [1; 2; 3; 4; 5; 6; 7; 8; 9;] s) >>= (fun m ->
            (kenzan s m (filter (filter [0; 1; 2; 3; 4; 5; 6; 7; 8; 9;] s) m)) >>= (fun e ->
            (filter (filter (filter [0; 1; 2; 3; 4; 5; 6; 7; 8; 9;] s) m) e) >>= (fun n ->
            (filter (filter (filter (filter [0; 1; 2; 3; 4; 5; 6; 7; 8; 9;] s) m) e) n) >>= (fun d ->
            (filter (filter (filter (filter (filter [0; 1; 2; 3; 4; 5; 6; 7; 8; 9;] s) m) e) n) d) >>= (fun o ->
            (filter (filter (filter (filter (filter (filter [0; 1; 2; 3; 4; 5; 6; 7; 8; 9;] s) m) e) n) d) o) >>= (fun r ->
            (filter (filter (filter (filter (filter (filter (filter [0; 1; 2; 3; 4; 5; 6; 7; 8; 9;] s) m) e) n) d) o) r) >>= (fun y ->
            (guard (money s e n d m o r y)) >>= (fun _ ->
                return (s, e, n, d, m, o, r, y))))))))));;
(** check if "banana + banana = sinamon" *)
Sys.time();;
let test_banana ba na si mo n =
  (100 * ba + 10 * na + na
   + 100 * ba + 10 * na + na
   = 1000 * si + 100 * na + 10 * mo + n)

(** check if "send + more = money" *)
let test_money s e n d m o r y =
  (1000 * s + 100 * e + 10 * n + d
   + 1000 * m + 100 * o + 10 * r + e
   = 10000 * m + 1000 * o + 100 * n + 10 * e + y)
  