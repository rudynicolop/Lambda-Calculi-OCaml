open Core
open Option
open Util
open FunUtil
open Syntax
open Sub

(** Dynamic semantics. *)

(** Call-by-value. *)
let rec cbv : b_term -> b_term option = function
  | App (Abs (_,_,e1), (Abs _ | TypAbs _ as e2)) ->
    some $ sub_term 0 0 e2 e1
  | App (Abs _ as e1, e2) -> cbv e2 >>| app e1
  | App (e1, e2) -> cbv e1 >>| fun e1' -> app e1' e2
  | TypApp (TypAbs (_,_,e), t) -> some $ sub_typ_term 0 t e
  | TypApp (e, t) -> cbv e >>| fun e' -> typapp e' t
  | _ -> None

(** Call-by-name. *)
let rec cbn : b_term -> b_term option = function
  | App (Abs (_,_,e1), e2) -> some $ sub_term 0 0 e2 e1
  | App (e1, e2) -> cbn e1 >>| fun e1' -> app e1' e2
  | TypApp (TypAbs (_,_,e), t) -> some $ sub_typ_term 0 t e
  | TypApp (e, t) -> cbn e >>| fun e' -> typapp e' t
  | _ -> None

(** No more beta-reductions. *)
let rec stuck : b_term -> bool = function
  | Var _ -> true
  | App (Abs _,_) | TypApp (TypAbs _,_) -> false
  | Abs (_,_,e)
  | TypAbs (_,_,e)
  | TypApp (e,_) -> stuck e
  | App (e1, e2) -> stuck e1 && stuck e2

(** Full normal-order. *)
let rec normal : b_term -> b_term option = function
  | Abs (_,t,e) -> normal e >>| abs () t
  | App (Abs (_,_,e1),e2) -> some $ sub_term 0 0 e2 e1
  | App (e1,e2) when stuck e1 -> normal e2 >>| app e1
  | App (e1,e2) -> normal e1 >>| fun e1' -> app e1' e2
  | TypAbs (_,k,e) -> normal e >>| typabs () k
  | TypApp (TypAbs (_,_,e),t) -> some $ sub_typ_term 0 t e
  | TypApp (e,t) -> normal e >>| fun e' -> typapp e' t
  | _ -> None

(** Full applicative-order. *)
let rec appl : b_term -> b_term option = function
  | Abs (_,t,e) -> appl e >>| abs () t
  | App (Abs (_,_,e1),e2)
    when stuck e1 && stuck e2 -> some $ sub_term 0 0 e2 e1
  | App (e1,e2) when stuck e1 -> appl e2 >>| app e1
  | App (e1,e2) -> appl e1 >>| fun e1' -> app e1' e2
  | TypAbs (_,k,e) -> appl e >>| typabs () k
  | TypApp (TypAbs (_,_,e),t)
    when stuck e -> some $ sub_typ_term 0 t e
  | TypApp (e,t) -> appl e >>| fun e' -> typapp e' t
  | _ -> None
