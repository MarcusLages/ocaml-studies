(** Set is used to keep track of visited vertices *)
module VSet = Set.Make(String)

(** [lift2_default] lifts a function [f] to an [option], but
    instead, if one [option] is [None] returns the other as [option]
    and if both are [Some] than [f] is applied to [o1] and [o2]
*)
let lift2_default f o1 o2 =
    match o1, o2 with
    | None, None -> None
    | Some _, None -> o1
    | None, Some _ -> o2
    | Some v1, Some v2 -> Some (f v1 v2)

(** [min_edge] returns the edge with the minimum weight between 2 edges
*)
let min_edge ((_, _, min_w_acc) as acc) ((_, _, min_w_next) as next) =
    if min_w_acc <= min_w_next then acc
    else next

(** [get_min_edge_opt] returns the option of an edge with the minimum 
    weight from a valid vertex named [v] in a [Graph.t g] and considering
    a [Set] with invalid and already visited vertices as [string];
    If a valid wan't found, returns [None]
*)
let get_min_edge_opt visited v g =
    match Graph.neighbours v g with
    | [] -> None
    | l ->
        let aux acc (v_next, w_next) = 
            match acc with
                | None -> Some (v, v_next, w_next)
                | Some acc_e ->
                    Some (min_edge acc_e (v, v_next, w_next))
        in
        List.fold_left aux None (
            List.filter (fun (dest, _) -> not (VSet.mem dest visited)) l
        )

(** [unvisited_min_edge_opt] returns the option of an edge with the minimum 
    weight from all the vertices in the [visited Set] in a [Graph.t g] and
    ignoring edges between 2 already visited vertices;
    If a valid wan't found, returns [None]
*)
let unvisited_min_edge_opt visited g =
    let min_edges = 
        VSet.fold (
            fun v acc' ->
                (get_min_edge_opt visited v g)::acc'
        ) visited []
    in
    List.fold_left (lift2_default min_edge) None min_edges

(** [min_tree] returns a minimum spanning tree in a [Graph.t g] starting
    from vertex [starter_v] using Prim's algorithm, returning a tuple
    with the list of [Graph.edge]'s and their total weight
*)
let min_tree starter_v g =
    let rec aux acc visited =
        match unvisited_min_edge_opt visited g with
        | None -> acc
        | Some ((v1, v2, w) as next) ->
            let tree_e, tree_w = acc in
            aux (next::tree_e, (tree_w + w)) (VSet.add v2 visited)
    in
    aux ([], 0) (VSet.singleton starter_v)

(** [parse_edge] parses a line as [string] into a [Graph.edge];
    If there's not enough data, returns a [Failure]
*)
let parse_edge line =
    let words =
        let aux acc x = 
            match x with
            | "" -> acc
            | _ -> x::acc
        in
        List.rev @@ List.fold_left aux [] (String.split_on_char ' ' line)
    in
    match words with
    | v1::v2::w::_ ->
        (v1, v2, (int_of_string w))
    | _ ->
        failwith "parse_edge: Invalid line to create an edge."
           
(** [read_data] receives a filename with edges to create a graph and
    returns a [Graph.t] created from those edges
*)
let read_data filename =
    let ic = open_in filename in
    let rec aux acc =
        try
            let edge = parse_edge (input_line ic)
            in
            aux (Graph.add_edge edge acc)
        with
        | End_of_file ->
            close_in ic;
            acc
        | Graph.Error e ->
            aux acc
        | Failure e ->
            aux acc
    in
    aux Graph.empty