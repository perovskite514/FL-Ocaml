type ('e, 'a) t = Reader of ('e -> 'a)

let run = function | Reader r -> r

let map f m = Reader (fun env -> f (run m env))

let bind f m = Reader (fun env -> run (f (run m env)) env)

let ( >>= ) m f = bind f m

let return x = Reader (fun _ -> x)

let ask () = Reader (fun env -> env)

let local f m = Reader (fun env -> run m (f env))

(* Example *)
type env = {
  name: string;
  age: int;
}

let to_string age name = 
  "name: " ^ name ^ "\nage: " ^ string_of_int age

let name env = env.name

let _ =
  let r =
    return 24
    |> map (fun x -> x + 1)
    >>= (fun x -> ask () |> map name |> map (to_string x))
    |> local (fun env -> {env with name="Vincent"})
  in
  let env = {name= "Jack"; age= 85} in
  Printf.printf "%s\n" (run r env)
(* name: Vincent *)
(* age: 25 *)

(* Example2 *)

let _ =
  let r =
    return 25
    |> map (fun x -> x + 10)
    >>= (fun x -> ask () |> map name |> map (to_string x))
    |> local (fun env -> {env with name="Sato"})
  in
  let env = {name= "Tanaka"; age= 20} in
  Printf.printf "%s\n" (run r env)

(* Example3 *)
type env1 = {
  meter: int;
  centimeter: int;
}

let to_string1 cm m = 
  string_of_int m  ^ "meter = " ^ string_of_int cm ^ "centimeter" 

let name1 env1 = env1.meter

let _ =
  let r =
    return 99
    |> map (fun x -> x + 1)
    >>= (fun x -> ask () |> map name1 |> map (to_string1 x))
    |> local (fun env -> {env with meter= 1})
  in
  let env1 = {meter= 100; centimeter= 10000} in
  Printf.printf "%s\n" (run r env1)

