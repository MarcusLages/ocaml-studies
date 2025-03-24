module VSet = Set.Make(String)

let lift2_default f o1 o2 =
    match o1, o2 with
    | None, None -> None
    | Some _, None -> o1
    | None, Some _ -> o2
    | Some v1, Some v2 -> Some (f v1 v2)

let min_edge ((_, _, min_w_acc) as acc) ((_, _, min_w_next) as next) =
    if min_w_acc <= min_w_next then acc
    else next

(* Think on how to check the previous *)
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

let unvisited_min_edge_opt visited g =
    let min_edges = 
        VSet.fold (
            fun v acc' ->
                (get_min_edge_opt visited v g)::acc'
        ) visited []
    in
    List.fold_left (lift2_default min_edge) None min_edges

let min_tree starter_v g =
    let rec aux acc visited =
        match unvisited_min_edge_opt visited g with
        | None -> acc
        | Some ((v1, v2, w) as next) ->
            let tree_e, tree_w = acc in
            aux (next::tree_e, (tree_w + w)) (VSet.add v2 visited)
    in
    aux ([], 0) (VSet.singleton starter_v)

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