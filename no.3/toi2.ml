module Mystack = struct

  exception Empty

  type 'a t = 'a list 

  let pop st = match st with
            | [] -> raise Empty
            | x :: res -> (x, res)

  let push x st = x :: st

  let empty = []

  let size st = List.length st  

end

module type MYSTACK = sig

  type 'a t

  val pop : 'a t -> ('a * 'a t)

  val push : 'a -> 'a t -> 'a t

  val empty : 'a t

  val size : 'a t -> int

end

module Stack : MYSTACK
        = Mystack;;

(**動作例用*)
let st = Stack.empty;;
let st = Stack.push 10 st;;
let st = Stack.push 5 st;;
let st = Stack.push 3 st;;
let i = Stack.size st;;
let st = Stack.pop st;;

