open Core
open Util
open FunUtil

(** Syntax. *)
type sort = Prop | Suc of sort

(** Term syntax. *)
type ('a,'b) term =
  | Sort of sort
  | Var of 'a
  | Abs of 'b * ('a,'b) term * ('a,'b) term
  | App of ('a,'b) term * ('a,'b) term
  | Pi of 'b * ('a,'b) term * ('a,'b) term

let sort s = Sort s
let var a = Var a
let abs b x y = Abs (b,x,y)
let app x y = App (x,y)
let pi b x y = Pi (b,x,y)

(** Parsed syntax. *)
type p_term = (string,string) term

(** De Bruijn syntax. *)
type b_term = (int,unit) term

let rec (=?) (s1 : sort) (s2 : sort) : bool =
  match s1, s2 with
  | Prop, Prop -> true
  | Suc s1, Suc s2 -> s1 =? s2
  | _, _ -> false

let rec string_of_sort = function
  | Prop -> "*"
  | Suc Prop -> "□"
  | Suc (Suc Prop) -> "∆"
  | Suc s -> "S " ^ string_of_sort s

(** Parsed syntax to De Bruijn Syntax
    [\x.\y.x -> \.\.1] *)
let rec b_of_p_term (ctx : string list) : p_term -> b_term =
  function
  | Sort s -> sort s
  | Var x -> var $ ListUtil.index_of_default String.(=) x ctx
  | Abs (x,t1,t2) -> abs () (b_of_p_term ctx t1) $ b_of_p_term (x :: ctx) t2
  | App (e1,e2)   -> app (b_of_p_term ctx e1) $ b_of_p_term ctx e2
  | Pi (x,t1,t2)  -> pi () (b_of_p_term ctx t1) $ b_of_p_term (x :: ctx) t2

(** De Bruijn to Parsed Syntax
    [\.\.1 -> \x1.\x2.x1] *)
let rec p_of_b_term (depth : int) : b_term -> p_term =
  function
  | Sort s -> sort s
  | Var n -> var $ "x" ^ string_of_int (depth - n)
  | Abs (_,t1,t2) ->
    abs
      ("x" ^ string_of_int (depth+1))
      (p_of_b_term depth t1)
    $ p_of_b_term (depth+1) t2
  | App (e1,e2) -> app (p_of_b_term depth e1) $ p_of_b_term depth e2
  | Pi (_,t1,t2) ->
    pi
      ("x" ^ string_of_int (depth+1))
      (p_of_b_term depth t1)
    $ p_of_b_term (depth+1) t2

let rec string_of_term (f : 'a -> string) (g : 'b -> string)
  : ('a,'b) term -> string =
  function
  | Sort s -> string_of_sort s
  | Var x  -> f x
  | Abs (x,t1,t2) ->
    "(λ" ^ g x ^ string_of_term f g t1 ^ "." ^ string_of_term f g t2 ^ ")"
  | Pi  (x,t1,t2) ->
    "(∏" ^ g x ^ string_of_term f g t1 ^ "." ^ string_of_term f g t2 ^ ")"
  | App (t1,t2) ->
    "(" ^ string_of_term f g t1 ^ " " ^ string_of_term f g t2  ^ ")"

let string_of_p_term : p_term -> string = string_of_term (fun x -> x ^ ":") id
let string_of_b_term : b_term -> string = string_of_term string_of_int $ consume ""
