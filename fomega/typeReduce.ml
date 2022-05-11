open Core
open Option
open Syntax
open Util
open FunUtil
open Sub

(** No further beta-reductions. *)
let rec stuck : b_typ -> bool = function
  | TVar _ -> true
  | TAbs (_,_,t) | TForall (_,_,t) -> stuck t
  | TApp (TAbs _,_) -> false
  | TApp (t1,t2)
  | TArrow (t1,t2) -> stuck t1 && stuck t2

let rec normal : b_typ -> b_typ option = function
  | TApp (TAbs (_,_,t1),t2) -> some $ sub_typ ~arg:t2 t1
  | TApp (t1,t2) when stuck t1 -> normal t2 >>| tapp t1
  | TApp (t1,t2) -> normal t1 >>| fun t1' -> tapp t1' t2
  | TAbs (_,k,t) -> normal t >>| tabs () k
  | TForall (_,k,t) -> normal t >>| tforall () k
  | TArrow (t1,t2) when stuck t1 -> normal t2 >>| tarrow t1
  | TArrow (t1,t2) -> normal t1 >>| fun t1' -> tarrow t1' t2
  | _ -> None

let normalize : b_typ -> b_typ =
  refl_trans_clos normal my_ignore
