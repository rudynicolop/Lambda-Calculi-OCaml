open Core
open Option
open Util

(** Lambda Calculus Abstract Syntax Trees *)

(** Syntax template. *)
type ('a,'b) expr = Var of 'a | Lam of 'b * ('a,'b) expr | App of ('a,'b) expr * ('a,'b) expr

(** Parsed Syntax. *)
type p_expr = (string,string) expr

(** De Bruijn Syntax. *)
type b_expr = (int,unit) expr

type ('a,'b) either = L of 'a | R of 'b

(** Call by need syntax for lazy evaluation. *)
type l_expr = ((int, int) either, unit) expr

(** Higher-Order Syntax for Closed Terms. *)
type h_expr =
  | HLam of (h_expr -> h_expr)
  | HApp of h_expr * h_expr
  | HPrintVar of string

(** Parsed syntax to De Bruijn Syntax
    [\x.\y.x -> \.\.1] *)
let rec b_expr_of_p_expr (ctx : string list) (e : p_expr) : b_expr =
  match e with
  | Var x ->
    begin match ListUtil.index_of String.equal x ctx with
      | None -> Var (List.length ctx + 1) (* free variable *)
      | Some n -> Var n (* bound variable *)
    end
  | Lam (x,e) -> Lam ((), b_expr_of_p_expr (x :: ctx) e)
  | App (e1,e2) -> App (b_expr_of_p_expr ctx e1, b_expr_of_p_expr ctx e2)

(** De Bruijn to Parsed Syntax
    [\.\.1 -> \x1.\x2.x1] *)
let rec p_expr_of_b_expr (depth : int) (e : b_expr) : p_expr =
  match e with
  | Var n -> Var ("x" ^ string_of_int (depth - n))
  | Lam (_,e) -> Lam ("x" ^ string_of_int (depth+1), p_expr_of_b_expr (depth+1) e)
  | App (e1,e2) -> App (p_expr_of_b_expr depth e1, p_expr_of_b_expr depth e2)

(** De Bruijn to HOAS:
    [\.\.1 -> fun x -> fun y -> x] *)
let rec h_expr_of_b_expr
    (stack : h_expr list) (e : b_expr) : h_expr option =
  match e with
  | Var n -> List.nth stack n
  | Lam (_,e) ->
    h_expr_of_b_expr (HPrintVar "" :: stack) e >>|
    fun _ -> HLam (fun x ->
        match h_expr_of_b_expr (x :: stack) e with
        | Some e -> e
        | None -> failwith "Impossible")
  | App (e1, e2) ->
    h_expr_of_b_expr stack e1 >>=
    fun e1 -> h_expr_of_b_expr stack e2 >>|
    fun e2 -> HApp (e1, e2)

let rec string_of_p_expr (e : p_expr) : string =
  match e with
  | Var x -> x
  | Lam (x,e) -> "(λ" ^ x ^ "." ^ (string_of_p_expr e) ^ ")"
  | App (e1,e2) -> "(" ^ string_of_p_expr e1 ^ " " ^ string_of_p_expr e2 ^ ")"

let rec string_of_b_expr (e : b_expr) : string =
  match e with
  | Var n -> string_of_int n
  | Lam (_,e) -> "(λ." ^ (string_of_b_expr e) ^ ")"
  | App (e1,e2) -> "(" ^ string_of_b_expr e1 ^ " " ^ string_of_b_expr e2 ^ ")"

let rec p_expr_of_h_expr (depth : int) (e : h_expr) : p_expr =
  match e with
  | HPrintVar x -> Var x
  | HLam f ->
    let x = "x" ^ string_of_int (depth + 1) in
    Lam (x,
         f (HPrintVar x)
         |> p_expr_of_h_expr (depth + 1))
  | HApp (e1,e2) -> App (p_expr_of_h_expr depth e1,
                         p_expr_of_h_expr depth e2)
