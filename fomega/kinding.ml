open Core
open Result
open Util
open FunUtil
open Syntax

(** Kinding errors. *)
type kind_error =
  | UnboundTypVar of kind list * int
  | IllegalTypApp of kind list * kind * b_typ * b_typ
  | KindMismatch of kind list * kind * kind * b_typ * b_typ
  | NotStarKind of kind list * kind list * kind * b_typ * b_typ

(** Kinding judgement. *)
let rec kinding (g: kind list)
  : b_typ -> (kind, kind_error) Result.t = function
  | TVar n ->
    begin match List.nth g n with
      | Some k -> return k
      | None -> fail $ UnboundTypVar (g,n)
    end
  | TAbs (_,k,t) ->
    kinding (k :: g) t >>| karrow k
  | TApp (t1,t2) ->
    kinding g t2 >>= fun k2 ->
    kinding g t1 >>=
    begin function
      | KArrow (k,k') when k =? k2 -> return k'
      | KArrow (k,_) -> fail $ KindMismatch (g,k,k2,t1,t2)
      | k1 -> fail $ IllegalTypApp (g,k1,t1,t2)
    end
  | TArrow (t1,t2) as ta ->
    kinding g t1 >>= fun k1 ->
    kinding g t2 >>= fun k2 ->
    begin match k1, k2 with
      | KStar, KStar -> return KStar
      | KStar, _ -> fail $ NotStarKind (g,g,k2,ta,t2)
      | _, _ -> fail $ NotStarKind (g,g,k1,ta,t1)
    end
  | TForall (_,k,t) as tf ->
    kinding (k :: g) t >>=
    begin function
      | KStar -> return KStar
      | k -> fail $ NotStarKind (g,k::g,k,tf,t)
    end    
