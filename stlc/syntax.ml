open Core
open Option

(** Simply-Typed Lambda Calculus Syntax *)

(** Types: *)
type typ = False | Arrow of typ * typ

(** Template of Term-Syntax: *)
type ('a,'b) expr =
  | Var of 'a
  | Abs of 'b * typ * ('a, 'b) expr
  | App of ('a, 'b) expr * ('a, 'b) expr

(** Higher-Order Syntax: *)
type 'a hoas =
  | HDebugVar of 'a
  | HAbs of ('a hoas -> 'a hoas)
  | HApp of 'a hoas * 'a hoas

(** Typed Syntax: *)
type ('a,'b,_) term =
  | TVar : 'a * 't -> ('a,'b,'t) term
  | TAbs : 'b * 't * ('a,'b,'r) term -> ('a,'b,'t -> 'r) term
  | TApp : ('a,'b,'t -> 'r) term * ('a,'b,'t) term -> ('a,'b,'r) term

(** Parsed terms. *)
type p_expr = (string,string) expr

(** De Bruijn syntax. *)
type b_expr = (int,unit) expr

(** Get index of (first occurence of) element in list *)
let rec index_of (eqb : 'a -> 'a -> bool) (a : 'a) (l : 'a list) : int option =
  match l with
  | [] -> None
  | h :: t -> if eqb h a then Some 0 else index_of eqb a t >>| fun n -> n + 1

(** Parsed to De Bruijn Syntax. *)
let rec b_of_p_expr (stk : string list) (e : p_expr) : b_expr =
  match e with
  | Var x ->
    begin match index_of String.equal x stk with
      | None -> Var (List.length stk + 1)
      | Some n -> Var n
    end
  | Abs (x,t,e) -> Abs ((), t, b_of_p_expr (x :: stk) e)
  | App (e1,e2) -> App (b_of_p_expr stk e1, b_of_p_expr stk e2)

(** De Bruijn to Parsed Syntax. *)
let rec p_of_b_expr (depth : int) (e : b_expr) : p_expr =
  match e with
  | Var n -> Var ("x" ^ string_of_int (depth - n))
  | Abs (_,t,e) -> Abs ("x" ^ string_of_int (depth + 1), t,
                        p_of_b_expr (depth + 1) e)
  | App (e1,e2) -> App (p_of_b_expr depth e1,
                        p_of_b_expr depth e2)

let rec string_of_typ (t : typ) : string =
  match t with
  | False -> "⊥"
  | Arrow (t1,t2) ->
    "(" ^ string_of_typ t1 ^ " → " ^ string_of_typ t2 ^ ")"

let rec string_of_p_expr (e : p_expr) : string =
  match e with
  | Var x -> x
  | Abs (x,t,e) ->
    "(λ" ^ x ^ ":" ^ string_of_typ t ^ "." ^ string_of_p_expr e ^ ")"
  | App (e1, e2) ->
    "(" ^ string_of_p_expr e1 ^ " " ^ string_of_p_expr e2 ^ ")"
