(* implementation of reference type *)
type 'a ref = {mutable contents: 'a}

let ref x = {contents = x}

let ( ! ) r = r.contents

let ( := ) r v = r.contents <- v

let incr r = r.contents <- r.contents + 1

let decr r = r.contents <- r.contents - 1
