let () =
    let last = Array.length Sys.argv - 1 in
    for i = 1 to last do
        Printf.printf (if i = last then "%s\n" else "%s") Sys.argv.(i) (* TErnary operator *)
    done