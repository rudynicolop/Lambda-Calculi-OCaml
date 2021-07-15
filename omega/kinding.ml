open Core
open Util
open FunUtil
open Syntax

type kind_error =
  | UnboundTypVar of kind list * int
  | IllegalTypApp of kind list * kind * b_typ * b_typ
  | KindMismatch of kind list * kind * kind * b_typ * b_typ
  | BadArrowKind of kind list * kind * b_typ * b_typ

let rec kinding (g: kind list)
  : b_typ -> (kind,kind_error) Result.t = function
  | TBot -> Result.return KStar
  | TVar n ->
    begin match List.nth g n with
      | Some k -> Result.return k
      | None -> Result.fail $ UnboundTypVar (g,n)
    end
  | TAbs (_,k,t) ->
    let open Result in
    kinding (k :: g) t >>| karrow k
  | TApp (t1,t2) ->
    let open Result in
    kinding g t2 >>= fun k2 ->
    kinding g t1 >>=
    begin function
      | KArrow (k,k') when k =? k2 -> return k'
      | KArrow (k,_) -> fail $ KindMismatch (g,k,k2,t1,t2)
      | KStar as k1 -> fail $ IllegalTypApp (g,k1,t1,t2)
    end
  | TArrow (t1,t2) as t ->
    let open Result in
    kinding g t1 >>= fun k1 ->
    kinding g t2 >>= fun k2 ->
    begin match k1,k2 with
      | KStar, KStar -> return KStar
      | KStar, _ -> fail $ BadArrowKind (g,k2,t,t2)
      | _, _ -> fail $ BadArrowKind (g,k2,t,t1)
    end
