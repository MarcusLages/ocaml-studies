let return x = (x, "")
let bind (x, s) f =
    let (x', s') = f x in
    (x', s ^ s')

let (>>=) = bind

let inc x = (x + 1, "inc " ^ string_of_int x ^ "; ")
let dec x = (x - 1, "dec " ^ string_of_int x ^ "; ")

let rec gcd a b =
    if b = 0 then a
    else gcd b (a mod b)

let tell s = ((), s)
let ( >> ) m1 m2 = m1 >>= fun _ -> m2

let example = tell "hello" >> return 1 >>= inc

let rec gcd' a b =
    if b = 0 then return a
    else tell (Printf.sprintf "gcd %d %d; " a b) >> gcd' b (a mod b)