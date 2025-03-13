(* This is a minimal test file.  One of its purposes is to show the order of
   the arguments to each function *) 
module M = Kvtree.Make(Int)

let t = M.of_list [(3, "three"); (2, "two"); (7, "seven"); (6, "six"); (8, "eight")]
let l = [(2, "two"); (3, "three"); (6, "six"); (7, "seven"); (8, "eight")]
let s = 
  "N(3, three, N(2, two, L, L), N(7, seven, N(6, six, L, L), N(8, eight, L, L)))"

let t2 = M.of_list [(3, 'a')]
let s2 = "N(3, a, L, L)"

let t3 = M.insert 2 'b' t2
let s3 = "N(3, a, N(2, b, L, L), L)"

let t4 = M.of_list [(3, "3"); (7, "7"); (5, "5"); (6, "6"); (8, "8"); (9, "9")] 
let s4 = "N(3, 3, L, N(7, 7, N(5, 5, L, N(6, 6, L, L)), N(8, 8, L, N(9, 9, L, L))))"
let t5 = M.delete 7 t4
let s5a = "N(3, 3, L, N(6, 6, N(5, 5, L, L), N(8, 8, L, N(9, 9, L, L))))"
let s5b = "N(3, 3, L, N(8, 8, N(5, 5, L, N(6, 6, L, L)), N(9, 9, L, L)))"

let run () =
  assert (M.find 7 t = "seven");
  assert (M.find_opt 7 t = Some "seven");
  assert (M.to_list t = l);
  assert (M.to_string (fun (k, v) -> Printf.sprintf "%d, %s" k v) t = s);
  assert (M.to_string (fun (k, v) -> Printf.sprintf "%d, %c" k v) t2 = s2);
  assert (M.to_string (fun (k, v) -> Printf.sprintf "%d, %c" k v) t3 = s3);
  assert (M.to_string (fun (k, v) -> Printf.sprintf "%d, %s" k v) t4 = s4);
  assert (
    let s = M.to_string (fun (k, v) -> Printf.sprintf "%d, %s" k v) t5 in
    s = s5a || s = s5b)