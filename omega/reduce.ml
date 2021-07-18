open Core
open Option
open Syntax
open Sub
open Util
open FunUtil

(** Dynamic Semantics. *)

(** Call-by-value. *)
let rec cbv : b_term -> b_term option = function
  | App (Abs (_,_,e1), (Var _ | Abs _ as e2)) ->
    some $ sub_term_top e2 e1
  | App ((Abs _ as e1), e2) -> cbv e2 >>| app e1
  | App (e1, e2) -> cbv e1 >>| fun e1' -> app e1' e2
  | _ -> None

(** Call-by-name. *)
let rec cbn : b_term -> b_term option = function
  | App (Abs (_,_,e1), e2) -> some $ sub_term_top e2 e1
  | App (e1, e2) -> cbn e1 >>| fun e1' -> app e1' e2
  | _ -> None

(** Stuckness: no more beta reductions. *)
let rec stuck : b_term -> bool = function
  | Var _ -> true
  | App (Abs _, _) -> false
  | App (e1, e2) -> stuck e1 && stuck e2
  | Abs (_,_,e) -> stuck e

(** Normal-order. *)
let rec normal : b_term -> b_term option = function
  | App (Abs (_,_,e1), e2) -> some $ sub_term_top e2 e1
  | App (e1, e2) when stuck e1 -> normal e2 >>| app e1
  | App (e1, e2) -> normal e1 >>| fun e1' -> app e1' e2
  | Abs (_,t,e) -> normal e >>| abs () t
  | _ -> None

(** Applicative-order. *)
let rec appl : b_term -> b_term option = function
  | App (Abs (_,_,e1), e2)
    when stuck e1 && stuck e2 -> some $ sub_term_top e2 e1
  | App (e1, e2) when stuck e1 -> appl e2 >>| app e1
  | App (e1, e2) -> appl e1 >>| fun e1' -> app e1' e2
  | Abs (_,t,e) -> appl e >>| abs () t
  | _ -> None
