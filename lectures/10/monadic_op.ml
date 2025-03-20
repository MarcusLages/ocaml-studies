(* Monadic parser combinator 
	Lexer
	Parser
	To create Abstract Syntax Tree - AST according to the syntax of
	the language to create tokens.

	Parser combinators are monadic and high order function
	Angstrom: .mli

	functor: <$> or fmap => ('a -> 'b) -> 'a t -> 'b t
	applicative: <*>, pure => ('a -> 'b) t -> 'a t -> 'b t
	monad: return, >>= or bind => => 'a t -> ('a -> 'b t) -> 'b t
*)

(* Functor
	This is just lift
*)
let (<$>) f mx =
	match mx with
	| None -> None
	| Some x -> Some (f x)

(* Applicative *)
let (<*>) mf mx =
	match mf with
	| None -> None
	| Some f -> f <$> mx

(* Bind *)
let (>>=) mx f =
	match mx with
	| None -> None
	| Some x -> f x

let square x = x*x

let lifted_square mx = square <$> mx


let lift f mx = f <$> mx
let lift2 f mx1 mx2 = f <$> mx1 <*> mx2

(* Higher precedence than any of the other ones *)
let ( *> ) mx my = 
	(fun _ y -> y) <$> mx <*> my

let ( <* ) mx my = 
	(fun x _ -> x) <$> mx <*> my

let (>>|) f mx = mx >>= f

let add x y = x + y
let x1 = add <$> Some 3 <* Some 4 <*> Some 5

