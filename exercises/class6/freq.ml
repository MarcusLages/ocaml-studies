module F = Map.Make(String)

let rec count tab =
    let word = Scanf.scanf " %s" (fun x -> x) in
    if word = "" then tab
    else count (F.update word (function
        | None -> Some 1 (* First time we have that word. *)
        | Some n -> Some (n + 1) (* Next times we see that word *)
    ) tab)
    
let () =
    F.empty |> count |> F.to_list
        |> List.iter (
            fun (w, c) -> Printf.printf "%s: %d\n" w c
        )