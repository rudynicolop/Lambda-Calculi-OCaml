open Core
open Option

(** Lambda Calculus Abstract Syntax Trees *)

(** Syntax template *)
type ('a,'b) expr = Var of 'a | Lam of 'b * ('a,'b) expr | App of ('a,'b) expr * ('a,'b) expr

(** Parsed syntax *)
type p_expr = (string,string) expr

(** De Bruijn syntax *)
type b_expr = (int,unit) expr

(** Higher-Order Syntax *)
type h_expr = HLam of (h_expr -> h_expr) | HApp of h_expr * h_expr

(** Get index of (first occurence of) element in list *)
let rec index_of (eqb : 'a -> 'a -> bool) (a : 'a) (l : 'a list) : int option =
  match l with
  | [] -> None
  | h :: t -> if eqb h a then Some 0 else index_of eqb a t >>| fun n -> n + 1

(** Parsed syntax to De Bruijn Syntax
\x.\y.x -> \.\.1
*)
let rec b_expr_of_p_expr (ctx : string list) (e : p_expr) : b_expr =
  match e with
  | Var x ->
    begin match index_of String.equal x ctx with
      | None -> Var (List.length ctx + 1) (* free variable *)
      | Some n -> Var n (* bound variable *)
    end
  | Lam (x,e) -> Lam ((), b_expr_of_p_expr (x :: ctx) e)
  | App (e1,e2) -> App (b_expr_of_p_expr ctx e1, b_expr_of_p_expr ctx e2)

(** De Bruijn to Parsed Syntax
    \.\.1 -> \x1.\x2.x1
*)
let rec p_expr_of_b_expr (depth : int) (e : b_expr) : p_expr =
  match e with
  | Var n -> Var ("x" ^ string_of_int (depth - n))
  | Lam (_,e) -> Lam ("x" ^ string_of_int (depth+1), p_expr_of_b_expr (depth+1) e)
  | App (e1,e2) -> App (p_expr_of_b_expr depth e1, p_expr_of_b_expr depth e2)

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
