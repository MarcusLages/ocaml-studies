let subset l =
    let aux acc x =
        match acc with
        | [] -> [x]::acc
        | _ ->
            [x]::(List.fold_left (fun acc_list x_list -> (x::x_list)::acc_list) acc acc)
    in
    []::List.fold_left aux [] l