module F = Map.Make(String)

let rec count tab =
  let word = Scanf.scanf " %s" (fun x -> x) in
  if word = "" then tab
  else count (F.update word (function 
    | None -> Some 1
    | Some n -> Some (n + 1)) tab)

let () =
  F.empty |> count |> F.to_list 
    |> List.iter (fun (w, n) -> Printf.printf "%s: %d\n" w n)
