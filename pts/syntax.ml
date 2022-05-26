open Core
open Util
open FunUtil

(** Syntax. *)
type sort = Prop | Suc of sort

(** Term syntax. *)
type ('a,'b,'c) term =
  | Sort of sort
  | Var of 'a
  | App of ('a,'b,'c) term * ('a,'b,'c) term
  | Abs of 'b * ('a,'b,'c) term * ('a,'b,'c) term
  | Pi of 'c * ('a,'b,'c) term * ('a,'b,'c) term

let sort s = Sort s
let var a = Var a
let abs b x y = Abs (b,x,y)
let app x y = App (x,y)
let pi b x y = Pi (b,x,y)

(** Parsed syntax. *)
type p_term = (string,string,string option) term

(** De Bruijn syntax. *)
type b_term = (int,unit,unit) term

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
let rec b_of_p_term (ctx : string option list) : p_term -> b_term =
  function
  | Sort s -> sort s
  | Var x ->
    ListUtil.index_of_options ~eq:String.(=) x ctx
    |> Option.value_map ~default:(List.length ctx) ~f:id
    |> var
  | App (e1,e2)   -> app (b_of_p_term ctx e1) $ b_of_p_term ctx e2
  | Abs (x,t1,t2) -> abs () (b_of_p_term ctx t1) $ b_of_p_term (Some x :: ctx) t2
  | Pi (o,t1,t2)  -> pi () (b_of_p_term ctx t1) $ b_of_p_term (o :: ctx) t2

let rec occurs_b_term (y : int) : b_term -> bool =
  function
  | Sort _ -> true
  | Var x -> x = y
  | App (t1,t2) -> occurs_b_term y t1 || occurs_b_term y t2
  | Abs (_,t1,t2)
  | Pi  (_,t1,t2) -> occurs_b_term y t1 || occurs_b_term (succ y) t2

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
      begin
        if occurs_b_term 0 t2 then
          Option.some $ "x" ^ string_of_int (depth+1)
        else None
      end
      (p_of_b_term depth t1)
    $ p_of_b_term (depth+1) t2

let rec string_of_term (f : 'a -> string) (g : 'b -> string) (h : 'c -> string)
  : ('a,'b,'c) term -> string =
  function
  | Sort s -> string_of_sort s
  | Var x  -> f x
  | Abs (x,t1,t2) ->
    "(λ" ^ g x ^ string_of_term f g h t1 ^ "." ^ string_of_term f g h t2 ^ ")"
  | Pi  (x,t1,t2) ->
    "(∏" ^ h x ^ string_of_term f g h t1 ^ "." ^ string_of_term f g h t2 ^ ")"
  | App (t1,t2) ->
    "(" ^ string_of_term f g h t1 ^ " " ^ string_of_term f g h t2  ^ ")"

let string_of_p_term : p_term -> string =
  string_of_term
    id (fun x -> x ^ ":") $
  ((fun x -> x ^ ":") >> Option.value_map ~f:id ~default:"_")
    
let string_of_b_term : b_term -> string =
  string_of_term string_of_int (consume "") $ consume ""

let rec occurs_p_term (y : string) : p_term -> bool =
  let open String in
  function
  | Sort _ -> false
  | Var x -> x = y
  | Abs (x,t1,t2)
  | Pi (Some x,t1,t2) -> occurs_p_term y t1 || if x = y then false else occurs_p_term y t2
  | Pi (None,t1,t2) ->occurs_p_term y t1 || occurs_p_term y t2
  | App (t1,t2) -> occurs_p_term y t1 || occurs_p_term y t2

let rec fancy_string_of_p_term : p_term -> string = function
  | Sort s -> string_of_sort s
  | Var x -> x
  | App (t1,t2) ->
    "(" ^ fancy_string_of_p_term t1 ^ " " ^ fancy_string_of_p_term t2  ^ ")"
  | Abs (x,t1,t2) ->
    "(λ" ^ x ^ ":" ^ fancy_string_of_p_term t1 ^ "." ^ fancy_string_of_p_term t2 ^ ")"
  | Pi (Some x,t1,t2) when occurs_p_term x t2 ->
    "(∀" ^ x ^ ":" ^ fancy_string_of_p_term t1 ^ "," ^ fancy_string_of_p_term t2 ^ ")"
  | Pi (_,t1,t2) ->
    "(" ^ fancy_string_of_p_term t1 ^ "→" ^ fancy_string_of_p_term t2 ^ ")"
