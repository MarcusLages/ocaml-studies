(* Record definition of bstree instead of union/sum type *)
type 'a t = L | N of {data: 'a; left: 'a t; right: 'a t}
exception Empty

let empty = L
let is_empty t = t = L
let rec insert x t =
    match t with
    | L -> N {data = x; left = L; right = L}
    | N {data; left; right} when x < data ->
        N {data; left = insert x; right}
    | N {data; left; right} when x > data ->
        N {data; left = left; right = insert x}
    | _ -> t

let of_list l =
    List.fold_left (Fun.flip insert) empty l

let rec max = function
    | L -> raise Empty
    | N {data; right = L} -> data
    | N {right} -> max right

let rec delete x t =
    match t with
    | L -> L
    | N {data; left; right} when x < data ->
        N {data; left = delete x left; right}
    | N {data; left; right} when x > data ->
        N {data; left; right = delete x right}
    | N {left = L; right} ->
        right
    | N {right = L; left} ->
        left
    | N {left; right} ->
        let max = max left in
        N {data = max; left = delete max left; right}

(* Prints 12 with a width of 7 characters. *)
let print_example = Printf.printf "%*d" 7 12

(* pr is the printer function; We don't know the type *)
let print pr t =
    let rec p level t =
        Printf.printf "%*s" (3 * level) "";
        match t with
        | L -> Printf.printf "-\n"
        | N {data; left; right} ->
            pr data;
            Printf.printf "\n";
            p (level + 1) left;
            p (level + 1) right
    in
    p 0 t