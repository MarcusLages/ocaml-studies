(** DO NOT MODIFY THIS FILE!! **)
type t                             (* the abstract graph type *)
type edge = string * string * int  (* the edge type *) 

exception Error of string  (* exception raised by some of the functions *)

val empty : t              (* the empty graph *)

(* adds an edge to a graph
   * raises an exception if edge is invalid (one or both of the vertices
     are empty strings, or the weight is not positive)
   * raises an exception when a conflicting edge is being added e.g. 
     empty |> add_edge ("A", "B", 1) |> add_edge ("B", "A", 2)
     or when an edge joining a vertex to itself is being added
   * a "no-op" when the "same" edge is added a second time
*)
val add_edge : edge -> t -> t

(* returns a graph from the specified list of edges; may raise an exception *)
val of_edges : edge list -> t

(* returns the list of all vertices in a graph sorted in ascending order *)
val vertices: t -> string list

(* returns whether a string is the name of a vertex in a graph *)
val is_vertex : string -> t -> bool

(* returns a sorted list of all edges in a graph; see write-up for example *)
val edges : t -> edge list 

(* returns sorted list of the neighbours of a specific vertex in a graph;
   each neighbour is represented by a (vertex, weight) pair and the list
   is sorted in ascending order of the neighbour vertices;
   returns an empty list if the given vertex is not in the graph *)
val neighbours : string -> t -> (string * int) list

