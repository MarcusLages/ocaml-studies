module VEMap = Map.Make(String)
type t = (string * int) list VEMap.t
type edge = string * string * int

exception Error of string

let empty = VEMap.empty

let add_edge (v1, v2, w) g =
    match v1, v2, w with
    | _  when v1 = v2 ->
        raise @@ Error "add_edge: cannot add an edge from a vertex to itself"
    | "", _, _ | _, "", _ ->
        raise @@ Error "add_edge: cannot add an edge from/to an empty vertex"
    | _ ->
        let add_to_list new_v new_w old_e =
            match old_e with
            | None -> Some [(new_v, new_w)]
            | Some l ->
                let check_valid_new_edge (cur_v, cur_w) =
                    if cur_v = new_v && cur_w != new_w then
                        raise @@ Error "add_edge: conflicting edge being added"
                    else
                        cur_v = new_v
                in
                if List.exists check_valid_new_edge l then Some l
                else Some ((new_v, new_w)::l)

        in
        g 
        |> VEMap.update v1 (add_to_list v2 w)
        |> VEMap.update v2 (add_to_list v1 w)

let of_edges l =
    List.fold_left (Fun.flip add_edge) empty l

let vertices g =
    List.sort String.compare @@ 
        VEMap.fold (fun v _  acc -> v::acc) g []

let is_vertex v g =
    match VEMap.find_opt v g with
    | Some _ -> true
    | None -> false

let edges g =
    List.sort (
        fun (v1, v2, w) (v1', v2', w') ->
            if v1 != v1' then String.compare v1 v1'
            else String.compare v2 v2'
    ) @@ VEMap.fold (
        fun v e acc -> 
            List.fold_left (
                fun acc' (v', w) -> (v, v', w)::acc'
            ) acc e
    ) g []

let neighbours v g =
    match VEMap.find_opt v g with
    | Some l ->
        List.sort (fun (v, _) (v', _) -> String.compare v v') l
    | None -> []

let test =
    of_edges [("D", "C", 5); ("A", "B", 1); ("A", "D", 3); ("A", "C", 4); ("B", "A", 1);
              ("C", "A", 4); ("C", "D", 5); ("D", "A", 3); ("D", "B", 2); ("B", "D", 2)]