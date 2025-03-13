let return x = [x]

(* List monad or list bind *)
let bind l f = List.concat_map f l

let (>>=) = bind

let guard cond l =
    if cond then l else []

let multiply_to n =
    List.init n ((+) 1) >>= fun x ->
        List.init n ((+) 1) >>= fun y ->
            guard (x * y = n) (return (x, y))

let (let*) = bind
let multiply_to' n =
    let* x = List.init n ((+) 1) in
    let* y = List.init n ((+) 1) in
    guard (x * y = n) (return (x, y))