(** [record] type to store record information from file *)
type record = {
    firstname: string;
    lastname: string;
    score: int
}

(** [make_record] creates a record by using the given inputs;
    Raises [Failure "invalid_score"] if {score} is not between
    0 and 100 inclusive
*)
let make_record firstname lastname score =
    if score < 0 || score > 100 then
        failwith "record: invalid score"
    else
        {firstname; lastname; score}

(** [parse] parses a line into a [record];
    Raises [Failure "parse_record_error"] if there was not
        enough data to create a record (less than 3 words)
    Raises [Failure "int_of_string"] if the third data in the
        line ([score]) is not an integer;
    Raises [Failure "invalid_score"] if the third data in the
        line ([score]) is not between 0 and 100 (inclusive)
*)
let parse line =
    match String.split_on_char ' ' line with
    | fname::lname::score::_ ->
        make_record fname lname @@ int_of_string score
    | _ ->
        failwith "parse_record_error"

(** [get_records] is used to read records from an [in_channel],
    and then parse and store them into a list of [record]'s *)
let rec get_records ic = 
    let rec aux acc =
        try
            let record = parse @@ read_line ()
            in
            aux @@ record::acc
        with
        | End_of_file -> 
            close_in ic;
            acc
        | Failure f ->
            aux acc
    in
    aux []

(** [insert_record] returns a list with the given [record]
    inserted in the right position in the [lst] according to
    [cmp]*)
let rec insert_record ~cmp record lst =
    match lst with
    | [] -> [record]
    | h::t ->
        if cmp record h <= 0 then
            record::lst
        else
        h::insert_record ~cmp record t

(** [sort_records] sorts the [record]s ordered by their score
    in decreasing order *)
let sort_records lst =
    let cmp a b =
        if a.score != b.score then
            Int.compare b.score a.score
        else
            if a.lastname != b.lastname then
                String.compare a.lastname b.lastname
            else
                String.compare a.firstname b.firstname
    in
    let rec aux l =
        match l with
        | [] -> []
        | h::t -> insert_record ~cmp:cmp h @@ aux t
    in aux lst

(** [output_records] outputs the [records] to the screen *)
let output_records lst =
    let aux acc elem =
        Printf.printf "%3d %s %s\n" elem.score elem.firstname elem.lastname
    in
    List.fold_left aux () lst

(** Initial function to run program and check for correct arguments
    and errors *)
let () =
    if Array.length Sys.argv = 1 then
        Printf.fprintf stderr "usage: %s <file>\n" Sys.argv.(0)
    else
        try
            let records = get_records (open_in Sys.argv.(1))
            in
            output_records @@ sort_records records
        with
        | Sys_error msg -> Printf.fprintf stderr "%s\n" msg
