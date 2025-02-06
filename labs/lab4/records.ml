(** [record] type to store record information from file *)
type record = {
    firstname: string;
    lastname: string;
    score: int
}

(** [make_record] creates a record by using the given inputs. *)
let make_record firstname lastname score =
    {firstname; lastname; score}

(** [parse] parses a line into a [record] *)
let parse line = make_record "asdf" "asdfas" 0

(** [get_records] is used to read records from an [in_channel],
    and then parse and store them into a list of [record]'s *)
let rec get_records ic = 
    let rec aux acc =
        try
            let record = parse @@ read_line ()
            in
            aux @@ record::acc
        with
        | End_of_file -> acc
    in
    aux []

(** [sort_records] sorts the [record]s ordered by their score
    in decreasing order *)
let sort_records lst = []

(** [output_records] outputs the [records] to the screen *)
let output_records lst = ()

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
