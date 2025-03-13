(* maybe monad *)
let return x = Some x

let bind mx f =
  match mx with
  | None -> None
  | Some x -> f x

let ( >>= ) = bind


let div a b =
  if b = 0 then None
  else Some (a / b)

(* How do we make the other arithmetic operations take optional values &
   return optional values? *) 

(*
let square' x = x * x  (* takes int, returns int *)

let square'' x = Some (x * x)  (* takes int, returns int option *)

let square mx = (* takes int option, returns int option *)
  mx >>= square''
*)

let square mx = mx >>= fun x -> return (x * x)  

(* lift a function f : 'a -> 'b to 'a option -> 'b option *)
let lift f mx = mx >>= fun x -> return (f x)

let cube' x = x * x * x

let cube = lift cube'

(*
let add' x y = x + y
let add' = fun x -> fun y -> x + y
*)

let add mx my = mx >>= fun x -> my >>= fun y -> return (x + y)

let lift2 f mx my =
  mx >>= fun x ->
    my >>= fun y ->
      return (f x y)

let ( + ) = lift2 Stdlib.( + )
let ( - ) = lift2 Stdlib.( - )
let ( * ) = lift2 Stdlib.( * )

let ( / ) mx my =
  mx >>= fun x ->
    my >>= fun y ->
      div x y
