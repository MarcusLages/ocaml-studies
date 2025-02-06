let rec number ic acc =
  try
    Printf.printf "%05d %s\n" acc (input_line ic);
    number ic (acc + 1)
  with
  | End_of_file -> close_in ic

let () =
  if Array.length Sys.argv = 1 then
    Printf.fprintf stderr "usage: %s <file>\n" Sys.argv.(0)
  else
    try
      number (open_in Sys.argv.(1)) 1
    with
    | Sys_error msg -> Printf.fprintf stderr "%s\n" msg


