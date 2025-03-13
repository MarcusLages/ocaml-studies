type 'a t = 'a list

exception Empty

let empty = []

let is_empty q = q = []

let enqueue x q = q @ [x]

let dequeue = function
  | [] -> raise Empty
  | _ :: q -> q

let front = function
  | [] -> raise Empty
  | x :: _ -> x

let front_opt = function
  | [] -> None
  | x :: _ -> Some x

let of_list l = l
