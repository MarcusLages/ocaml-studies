(* Marcus Vinicius Santos Lages
    A01392327
*)

(** [parse_min_tree] receives a tuple with a list of [Graph.edge]'s
    and total weight of them and returns a formatted version of them
    as a [string]
*)
let format_min_tree (edges, total_w) =
    let edges_str =
        List.fold_right (
            fun (v1, v2, w) acc ->
                acc ^ "<" ^ v1 ^ ", " ^ v2 ^ ", " ^ (string_of_int w) ^ "> "
        ) edges ""
    in
    edges_str ^ "(weight: " ^ (string_of_int total_w) ^ ")\n"

(** Programs entry point *)
let () =
    if Array.length Sys.argv < 3 then
        Printf.fprintf stderr "usage: %s filename starting_vertex\n" Sys.argv.(0)
    else
        Sys.argv.(1)
        |> Util.read_data
        |> Util.min_tree (Sys.argv.(2))
        |> format_min_tree
        |> Printf.printf "%s"