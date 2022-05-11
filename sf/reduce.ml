open Core
open Option
open Syntax
open Sub
open Util
open FunUtil

(** Dynamic Semantics. *)

(** Call by value. *)
let rec cbv : b_expr -> b_expr option = function
  | App (Abs (_,_,e1),
         (Var _ | Abs _ | TypAbs _ as e2)) ->
    some $ sub ~arg:e2 e1
  | App ((Abs _ as e1), e2) -> cbv e2 >>| app e1
  | App (e1, e2) -> cbv e1 >>| fun e1' -> app e1' e2
  | TypApp (TypAbs (_,e), t) -> some $ sub_typ_term e t
  | TypApp (e, t) -> cbv e >>| fun e' -> typapp e' t
  | _ -> None

(** Call by name. *)
let rec cbn : b_expr -> b_expr option = function
  | App (Abs (_,_,e1), e2) -> some $ sub ~arg:e2 e1
  | App (e1, e2) -> cbn e1 >>| fun e1' -> app e1' e2
  | TypApp (TypAbs (_,e), t) -> some $ sub_typ_term e t
  | TypApp (e, t) -> cbn e >>| fun e' -> typapp e' t
  | _ -> None

(** Normal-order. *)
let rec normal : b_expr -> b_expr option = function
  | Abs (_,t,e) -> normal e >>| abs () t
  | App (Abs (_,_,e1),e2) -> some $ sub ~arg:e2 e1
  | App (e1,e2) ->
    begin match normal e1 with
      | None -> normal e2 >>| app e1
      | Some e1' -> some $ app e1' e2
    end
  | TypAbs (_,e) -> normal e >>| typabs ()
  | TypApp (TypAbs (_,e),t) -> some $ sub_typ_term e t
  | TypApp (e,t) -> normal e >>| fun e' -> typapp e' t
  | _ -> None

(** Applicative-order stuck-ness. *)
let rec stuck : b_expr -> bool = function
  | Var _ -> true
  | App (Abs _, _)
  | TypApp (TypAbs _, _) -> false
  | App ((Var _
         | App _
         | TypAbs _
         | TypApp _ as e1), e2) -> stuck e1 && stuck e2
  | Abs (_,_,e)
  | TypAbs (_,e)
  | TypApp ((Var _
            | Abs _
            | App _
            | TypApp _ as e),_) -> stuck e

(** Applicative-order. *)
let rec appl : b_expr -> b_expr option = function
  | Abs (_,t,e) -> appl e >>| abs () t
  | App (Abs (_,_,e1),e2) when stuck e1 && stuck e2 ->
    some $ sub ~arg:e2 e1
  | App (e1,e2) when stuck e1 -> appl e2 >>| app e1
  | App (e1,e2) -> appl e1 >>| fun e1' -> app e1' e2
  | TypAbs (_,e) -> appl e >>| typabs ()
  | TypApp (TypAbs (_,e),t) when stuck e ->
    some $ sub_typ_term e t
  | TypApp (e,t) -> appl e >>| fun e' -> typapp e' t
  | _ -> None
