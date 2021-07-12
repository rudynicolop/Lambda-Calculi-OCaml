open Core
open Util

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

(** Type equality. *)
let rec (=?) (a : typ) (b : typ) : bool =
  match a, b with
  | False, False -> true
  | Arrow (a1, a2), Arrow (b1, b2) ->
    a1 =? b1 && a2 =? b2
  | _, _ -> false

(** Parsed to De Bruijn Syntax. *)
let rec b_of_p_expr (stk : string list) : p_expr -> b_expr = function
  | Var x ->
    begin match ListUtil.index_of String.equal x stk with
      | None -> Var (List.length stk + 1)
      | Some n -> Var n
    end
  | Abs (x,t,e) -> Abs ((), t, b_of_p_expr (x :: stk) e)
  | App (e1,e2) -> App (b_of_p_expr stk e1, b_of_p_expr stk e2)

(** De Bruijn to Parsed Syntax. *)
let rec p_of_b_expr (depth : int) : b_expr -> p_expr = function
  | Var n -> Var ("x" ^ string_of_int (depth - n))
  | Abs (_,t,e) -> Abs ("x" ^ string_of_int (depth + 1), t,
                        p_of_b_expr (depth + 1) e)
  | App (e1,e2) -> App (p_of_b_expr depth e1,
                        p_of_b_expr depth e2)

let rec string_of_typ : typ -> string = function
  | False -> "⊥"
  | Arrow (t1,t2) ->
    "(" ^ string_of_typ t1 ^ " → " ^ string_of_typ t2 ^ ")"

let rec string_of_b_expr : b_expr -> string = function
  | Var n -> string_of_int n
  | Abs (_,t,e) ->
    "(λ" ^ string_of_typ t ^ "." ^ string_of_b_expr e ^ ")"
  | App (e1, e2) ->
    "(" ^ string_of_b_expr e1 ^ " " ^ string_of_b_expr e2 ^ ")"

let rec string_of_p_expr : p_expr -> string = function
  | Var x -> x
  | Abs (x,t,e) ->
    "(λ" ^ x ^ ":" ^ string_of_typ t ^ "." ^ string_of_p_expr e ^ ")"
  | App (e1, e2) ->
    "(" ^ string_of_p_expr e1 ^ " " ^ string_of_p_expr e2 ^ ")"
