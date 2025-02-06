let rec sum acc =
    try
        sum (Scanf.scanf " %d" @@ fun x -> x + acc)
    with
    | Scanf.Scan_failure _ -> (* Skip non integers *)
        ignore @@ Scanf.scanf " %s" (fun _-> ());
        sum acc
    | End_of_file -> acc

let () =
    Printf.printf "%d\n" @@ sum 0