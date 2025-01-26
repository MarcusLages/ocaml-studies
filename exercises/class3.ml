(* Anonymous functions *)
(* fun x -> x * x *)
(fun x -> x * x) 5
let square = fun x -> x * x

let add x y = x + y
let add x = fun y -> x + y (* Closure *)
let add = fun x -> fun y -> x + y

let rec map f l =
  match l with
  | [] -> []
  | h::t -> f h :: map f t

let map_tr f l =
    let rec aux acc l =
        match l with
        | [] -> List.rev acc
        | h::t -> aux (f h :: acc) t
    in
    aux [] l

let squared_list = map (fun x -> x * x) [1;2;3;4;5;6]

let squared_list = map (fun x -> x * x) [1;2;3;4;5;6]
let minus_list = map (( + ) 1) [1;2;3;4;5] (* [0;-1;-2...] *)

let flip f x y = f y x

let rec filter f = function (* fun l -> match l with *)
    | [] -> []
    | h::t ->
        if f h then h :: filter f t
        else filter f t

let filtered_list = filter (fun x -> x mod 2 = 0) [3;2;7;6;8]

let filter_tr f l =
    let rec aux acc = function
    | [] -> List.rev acc
    | h::t ->
        if f h then aux (h::acc) t
        else aux acc t
    in
    aux [] l

let rec take_while f l =
    match l with
    | [] -> []
    | h::t ->
        if f h then h::take_while f l
        else []

(* Insertion sort *)

(**
    Insert x into l keeping the order.
*)
let rec insert1 x l =
    match l with
    | [] -> [x]
    | y::t ->
        if x <= y then x::l
        else y :: insert1 x t

let rec sort1 l =
    match l with
    | [] -> []
    | h::t -> insert1 h @@ sort1 t

let rec insert2 cmp x l =
    match l with
    | [] -> [x]
    | y::t when cmp x y <= 0 -> x::l
    | y::t -> y::insert2 cmp x t

let rec sort2 cmp l =
    match l with
    | [] -> []
    | h::t -> insert2 cmp h @@ sort2 cmp t

let sorted_list = sort2 compare [2;4;6;1;4;5;7;3;2]
let c1 = compare 3 1
let c2 = String.compare "hello" " world"
let sorted_asc_list = sort2 String.compare ["hello"; "world"; "goodbye"]
let sorted_desc_list = sort2 (Fun.flip String.compare) ["hello"; "world"; "goodbye"]

let rec insert ~cmp x l =   (* When you pass a labeled arg, you need a label then. *)
    match l with
    | [] -> [x]
    | h::t ->
        if cmp x h <= 0 then x::l   (* When you use a labeled arg, you don't need the label*)
        else h::insert ~cmp x t

let rec sort ~cmp l =
    match l with
    | [] -> []
    | h::t -> insert ~cmp h @@ sort ~cmp t

let sorted_list2 = sort ~cmp:compare [3;1;2;5;3;6;3;2;5]
let sorted_list2 = sort [3;1;2;5;3;6;3;2;5] ~cmp:compare (* Labels allow order change *)

let rec fold_left f acc l = (* Stream reduce function from java *)
    match l with
    | [] -> acc
    | h::t -> fold_left f (f acc h) t
let summation = fold_left (+) 0 [4;2;1;6;3;4;2]
let reversed_list = fold_left (fun acc x -> x::acc) [] [3;1;2;5;2;3;5;1;4]

let rec fold_right f l acc =
    match l with
    | [] -> acc
    | h::t -> f h (fold_right f t acc)
let not_reversed_list = fold_right (fun x acc -> x::acc) [3;1;2;5;2;3;5;1;4] []
    
(** Practice average. *)
let average lst =
    match lst with
    | [] -> failwith "Cannot compute average of an empty list"
    | _ ->
        let sum, count =
            List.fold_left (fun (s, c) x -> (s +. x, c + 1)) (0.0, 0) lst
        in
        sum /. float_of_int count;;
    
    (* Example usage *)
    let lst = [1.0; 2.0; 3.0; 4.0; 5.0];;
    let avg = average lst;;
    print_float avg;; (* Output: 3.0 *)
      

let sort3 l = fold_left (Fun.flip insert1) [] l
let sort4 l = List.fold_right insert1 l [] (* Not tail recursive *)