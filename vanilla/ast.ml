(* open Core *)

(** Lambda Calculus Abstract Syntax Trees *)

(** Syntax template *)
type ('a,'b) expr = Var of 'a | Lam of 'b * ('a,'b) expr | App of ('a,'b) expr * ('a,'b) expr

(** Parsed syntax *)
type p_expr = (string,string) expr

(** De Bruijn syntax *)
type b_expr = (int,unit) expr

(** Higher-Order Syntax *)
type h_expr = HLam of (h_expr -> h_expr) | HApp of h_expr * h_expr
