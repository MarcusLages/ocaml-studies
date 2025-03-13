(* Binary search tree *)
type 'a t = Leaf | Node of 'a * 'a t * 'a t
let empty = Leaf
let is_empty t = t = Leaf

let rec size t =
    match t with
    | Leaf -> 0
    | Node (_, l, r) -> 1 + size l + size r

let rec insert x t =
    match t with
    | Leaf -> Node (x, Leaf, Leaf)
    | Node (v, l, r) when x < v ->
        Node (v, insert v l, r)
    | Node (v, l, r) when x > v ->
        Node (v, l, insert v r)
    | _ -> t

let of_list l =
    List.fold_left (Fun.flip insert) empty l

let rec search x t =
    match t with
    | Leaf -> false
    | Node (v, l, r) when x < v ->
        search x l
    | Node (v, l, r) when x > v ->
        search x r
    | _ -> true

let rec max t =
    match t with
    | Leaf -> failwith "max: empty tree"
    | Node (v, _, Leaf) -> v
    | Node (_, _, r) -> max r

let rec delete x t =
    match t with
    | Leaf -> Leaf
    | Node (v, l, r) when x < v ->
        Node (v, (delete x l), r)
    | Node (v, l, r) when x > v ->
        Node (v, l, (delete x r))
    | Node (_, l, Leaf) ->
        l
    | Node (_, Leaf, r) ->
        r
    | Node (_, l, r) ->
        let max = max l in
        Node (max, delete max l, r)

(* Module compilation
        ocamlbuild bstree.cmo
    On utop:
        #directory "_build";;
        #load "bstree.cmo";;
    Now, everytime you use a function or type, you will need to use
    Bstree._____

    You can use: "open Bstree" to avoid using the name
    (Like "using namespace std" in C++. Just like C++ it's also bad
    practice.)

    You may also use a "local open"
    Instead of:
        let t = Bstree.empty |> Bstree.insert 3 |> Bstree.insert 2
    Use:
        let t = Bstree.(empty |> insert 3 |> insert 2)
        or 
        let open Bstree in
            ...

    Standard: use the type defined by a module as "t"

*)