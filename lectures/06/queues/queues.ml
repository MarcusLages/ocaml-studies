module ListQueue = struct
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
end

module TwoListQueue = struct
  type 'a t = 'a list * 'a list
  exception Empty

  let empty = ([], [])

  (* implementation will ensure that queue is empty iff the 1st list is empty *)
  let is_empty (l, _) = l = []

  let enqueue x = function
    | [], l -> [x], l
    | l, l' -> l, x :: l'

  let dequeue = function
    | [], _ -> raise Empty
    | [_], l -> List.rev l, []
    | _ :: l, l' -> l, l'

  let front = function
    | [], _ -> raise Empty
    | x :: _, _ -> x

  let front_opt = function
    | [], _ -> None
    | x :: _, _ -> Some x
end
