(* Array.make;; *)
let ar = Array.make 10 true

let isqrt n =
    n |> float_of_int |> sqrt |> int_of_float

(* [primes n] returns a list of primes strictly less than [n];
    [n] must be bigger than 1;
    The imperative features make it weirder and break functional programming,
    but they tend to be more efficient*)
let primes n =
    let is_prime = Array.make n true in
    for i = 2 to isqrt n do
        if is_prime.(i) then
            let j = ref i in
            while (i * !j < n) do
                is_prime.(i * !j) <- false;
                incr j
            done
    done;
    is_prime.(0) <- false;
    is_prime.(1) <- false;
    is_prime |> Array.to_list |> List.mapi (fun i b -> (i, b)) |>
    List.filter_map (fun (i, b) -> if b then Some i else None)