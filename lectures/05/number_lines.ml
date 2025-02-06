let rec number acc =
  try
    let line = read_line () in
    Printf.printf "%05d %s\n" acc line;
    number (acc + 1)
  with
  | End_of_file -> ()

let () = number 1
