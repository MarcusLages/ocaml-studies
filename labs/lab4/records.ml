type record = {firstname: string; lastname: string; score: int}

let rec get_records_list ic = 
    let aux acc = ()
    in
    aux []

let parse line = []

let sort_records lst = []

let write_records lst = ()

let () =
    if Array.length Sys.argv = 1 then
        Printf.fprintf stderr "usage: %s <file>\n" Sys.argv.(0)
    else
        try
            let records = get_records_list (open_in Sys.argv.(1))
            in
            write_records @@ sort_records records
        with
        | Sys_error msg -> Printf.fprintf stderr "%s\n" msg
