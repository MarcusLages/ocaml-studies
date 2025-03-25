module VEMap = Map.Make(String)

(** [Graph.t] is represented by a [Map] of [string] as the vertex name to a
    [list] of [string * int] tuples representing the edges of that vertex, 
    with the first element being the name of the next vertex and the second
    as the weight of the edge 
*)
type t = (string * int) list VEMap.t

(** [Graph.edge] represents an edge between two vertices in the format of
    [src_vertex * dest_vertex * weight]
*)
type edge = string * string * int

(** exception raised by some of the functions *)
exception Error of string

(* [empty] is the empty graph *)
let empty = VEMap.empty

(** [add_edge] adds an edge to a graph [g]
   * raises an exception if edge is invalid (one or both of the vertices
     are empty strings, or the weight is not positive)
   * raises an exception when a conflicting edge is being added e.g. 
     empty |> add_edge ("A", "B", 1) |> add_edge ("B", "A", 2)
     or when an edge joining a vertex to itself is being added
   * a "no-op" when the "same" edge is added a second time
*)
let add_edge (v1, v2, w) g =
    match v1, v2, w with
    | _  when v1 = v2 ->
        raise @@ Error "add_edge: cannot add an edge from a vertex to itself"
    | _  when w <= 0 ->
        raise @@ Error "add_edge: cannot add an edge non positive weight"
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

(** [of_edges] returns a graph from the specified list of edges; may raise an exception *)
let of_edges l =
    List.fold_left (Fun.flip add_edge) empty l

(** [vertices] eturns the list of all vertices in a graph sorted in ascending order *)
let vertices g =
    List.sort String.compare @@ 
        VEMap.fold (fun v _  acc -> v::acc) g []

(** [is_vertex] eturns whether a string is the name of a vertex in a graph *)
let is_vertex v g =
    match VEMap.find_opt v g with
    | Some _ -> true
    | None -> false

(** [edges] returns a sorted list of all edges in a graph; see write-up for example *)
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

(** [neighbours] returns sorted list of the neighbours of a specific vertex in a graph;
   each neighbour is represented by a (vertex, weight) pair and the list
   is sorted in ascending order of the neighbour vertices;
   returns an empty list if the given vertex is not in the graph *)
let neighbours v g =
    match VEMap.find_opt v g with
    | Some l ->
        List.sort (fun (v, _) (v', _) -> String.compare v v') l
    | None -> []