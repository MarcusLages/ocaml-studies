let rec count acc =
  try
    ignore @@ read_line ();
    count (acc + 1)
  with
  | End_of_file -> acc

let () =
  Printf.printf "%d\n" @@ count 0

