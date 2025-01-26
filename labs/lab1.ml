(** [zip_ntr lst1 lst2] returns a list of tuples pairing elements
    of [lst1] and [lst2] of same 'index' into a tuple;
    Not tail-recursive
*)
let rec zip_ntr lst1 lst2 =
    match lst1, lst2 with
    | h1::t1, h2::t2 -> (h1, h2)::zip_ntr t1 t2
    | _ -> []
    
(** [zip_tr lst1 lst2] returns a list of tuples pairing elements
    of [lst1] and [lst2] of same 'index' into a tuple;
    Tail-recursive
*)
let rec zip_tr lst1 lst2 =
    let rec aux acc lst1 lst2 =
        match lst1, lst2 with
        | h1::t1, h2::t2 -> aux ((h1, h2)::acc) t1 t2
        | _ -> List.rev acc
    in
    aux [] lst1 lst2

(**/**)
let test_zip_ntr () =
    assert (zip_ntr [] [] = []);
    assert (zip_ntr [1] ["hello"] = [(1, "hello")]);
    assert (zip_ntr [1; 2; 3] ["hello"; "world"; "goodbye"] = [(1, "hello"); (2, "world"); (3, "goodbye")]);
    assert (zip_ntr [1; 2; 3] ["hello"; "world"] = [(1, "hello"); (2, "world")]);
    assert (zip_ntr [1; 2] ["hello"; "world"; "goodbye"] = [(1, "hello"); (2, "world")])
    
let test_zip_tr () =
    assert (zip_tr [] [] = []);
    assert (zip_tr [1] ["hello"] = [(1, "hello")]);
    assert (zip_tr [1; 2; 3] ["hello"; "world"; "goodbye"] = [(1, "hello"); (2, "world"); (3, "goodbye")]);
    assert (zip_tr [1; 2; 3] ["hello"; "world"] = [(1, "hello"); (2, "world")]);
    assert (zip_tr [1; 2] ["hello"; "world"; "goodbye"] = [(1, "hello"); (2, "world")])
(**/**)
    
(** [unzip_ntr lst] receives a list of pairs [lst] and returns a pair of lists
    which unzips each pair of [lst] in which the first component is put into the
    first list and second component is put into the second list in the same index
    they were in the input;
    Not tail-recursive
*)
let rec unzip_ntr lst =
    match lst with
    | (h1, h2)::t2 ->
        let l1, l2 = unzip_ntr t2 in
        h1::l1, h2::l2
    | _ -> ([],[])
    
(** [unzip_tr lst] receives a list of pairs [lst] and returns a pair of lists
    which unzips each pair of [lst] in which the first component is put into the
    first list and second component is put into the second list in the same index
    they were in the input;
    Tail-recursive
*)
let unzip_tr lst =
    let rec aux acc lst =
        match lst with
        | (h1, h2)::t2 ->
            aux (let l1, l2 = acc in h1::l1, h2::l2) t2
        | _ ->
            let lst1, lst2 = acc in
            (List.rev lst1, List.rev lst2)
    in
    aux ([],[]) lst

(**/**)
let test_unzip_ntr () =
    assert (unzip_ntr [] = ([],[]));
    assert (unzip_ntr [(1, "hello")] = ([1], ["hello"]) );
    assert (unzip_ntr [(1, "hello"); (2, "world"); (3, "goodbye")] = ([1; 2; 3], ["hello"; "world"; "goodbye"]));
    assert (unzip_ntr [(1, "hello"); (2, "world")] = ([1; 2], ["hello"; "world"]))

let test_unzip_tr () =
    assert (unzip_tr [] = ([],[]));
    assert (unzip_tr [(1, "hello")] = ([1], ["hello"]) );
    assert (unzip_tr [(1, "hello"); (2, "world"); (3, "goodbye")] = ([1; 2; 3], ["hello"; "world"; "goodbye"]));
    assert (unzip_tr [(1, "hello"); (2, "world")] = ([1; 2], ["hello"; "world"]))
(**/**)

(** [dedup_ntr lst] returns a list that has all the consecutive duplicates
    from [lst] removed;
    Not tail-recursive
*)
let rec dedup_ntr lst =
    match lst with
    | x1::x2::t when x1 = x2 ->
        dedup_ntr (x2::t)
    | h::t -> h::dedup_ntr t
    | _ -> []
    
(** [dedup_tr lst] returns a list that has all the consecutive duplicates
    from [lst] removed;
    Tail-recursive
*)
let rec dedup_tr lst =
    let rec aux acc lst =
        match lst with
        | x1::x2::t when x1 = x2 ->
            aux acc (x2::t)
        | h::t -> aux (h::acc) t
        | _ -> List.rev acc
    in aux [] lst

(**/**)
let test_dedup_ntr () =
    assert (dedup_ntr [] = []);
    assert (dedup_ntr [1;1;2;3;4;4;4;5] = [1;2;3;4;5]);
    assert (dedup_ntr [1;1;2;2;3;3;4;4;5;5] = [1;2;3;4;5]);
    assert (dedup_ntr [1;1;1;1;1;1;1] = [1]);
    assert (dedup_ntr [1;2;3;4;5] = [1;2;3;4;5])

let test_dedup_tr () =
    assert (dedup_tr [] = []);
    assert (dedup_tr [1;1;2;3;4;4;4;5] = [1;2;3;4;5]);
    assert (dedup_tr [1;1;2;2;3;3;4;4;5;5] = [1;2;3;4;5]);
    assert (dedup_tr [1;1;1;1;1;1;1] = [1]);
    assert (dedup_tr [1;2;3;4;5] = [1;2;3;4;5])
(**/**)

(** [split_last_ntr] returns a pair with the first element being a list
    with all the elements of [lst] with the exception of the last one
    and the second element of the tuple is the last element of [lst];
    Not tail-recursive
*)
let rec split_last_ntr lst = 
    match lst with
    | [] -> None
    | [t] -> Some ([], t)
    | h::t ->
        match split_last_ntr t with
        | Some (return_lst, tail) -> Some (h::return_lst, tail)
        | _ -> None

(** [split_last_tr] returns a pair with the first element being a list
    with all the elements of [lst] with the exception of the last one
    and the second element of the tuple is the last element of [lst];
    Not tail-recursive
*)
let split_last_tr lst = 
    let rec aux acc lst =
        match lst with
        | [] -> None
        | [t] -> Some (List.rev acc, t)
        | h::t -> aux (h::acc) t
    in
    aux [] lst

(**/**)
let test_split_last_ntr () =
    assert (split_last_ntr [] = None);
    assert (split_last_ntr [1] = Some ([], 1));
    assert (split_last_ntr [1;2;3;4] = Some ([1;2;3], 4));
    assert (split_last_ntr [14;232] = Some ([14], 232))

let test_split_last_tr () =
    assert (split_last_tr [] = None);
    assert (split_last_tr [1] = Some ([], 1));
    assert (split_last_tr [1;2;3;4] = Some ([1;2;3], 4));
    assert (split_last_tr [14;232] = Some ([14], 232))

let test_all () =
    test_zip_ntr ();
    test_zip_tr ();
    test_unzip_ntr ();
    test_unzip_tr ();
    test_dedup_ntr ();
    test_dedup_tr ();
    test_split_last_ntr ();
    test_split_last_tr ()
(**/**)
