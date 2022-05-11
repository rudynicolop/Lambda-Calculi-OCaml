open Core
open Option
open Util
open FunUtil
open Syntax
open Sub

(** Type-equivalence based on
    weak-head normalization of types.
    From Advanced Topics in
    Types & Programming Languages. *)

(** Weak-head reduction of types. *)
let rec weak_red : b_typ -> b_typ option =
  function
  | TApp (TAbs (_,_,t1), t2) -> some $ sub_typ ~arg:t2 t1
  | TApp (t1, t2) ->
    weak_red t1 >>| fun t1' -> tapp t1' t2
  | TArrow (t1, t2) ->
    begin match weak_red t1 with
      | Some t1' -> some $ tarrow t1' t2
      | None     -> weak_red t2 >>| tarrow t1
    end
  | _ -> None

(** Weak-head normalization of types. *)
let weak_norm : b_typ -> b_typ =
  refl_trans_clos weak_red my_ignore

let rec (==) (s : b_typ) (t : b_typ) : bool =
  typ_equiv_wf (weak_norm s) (weak_norm t)

and typ_equiv_wf (s : b_typ) (t : b_typ) : bool =
  match s, t with
  | TBot, TBot -> true
  | TVar x, TVar y -> x = y
  | TApp (s1, s2), TApp (t1, t2)
  | TArrow (s1, s2), TArrow (t1, t2) ->
    typ_equiv_wf s1 t1 && typ_equiv_wf s2 t2
  | TAbs (_,k,s), TAbs (_,k',t) -> k =? k' && s == t
  | s, TAbs (_,_,t) | TAbs (_,_,t), s ->
    tapp (rename_typ ((+) 1) s) (tvar 0) == t
  | _, _ -> false
    
(*
(** Term equivalence of types. *)
let rec typ_equiv (g : kind list) (s : b_typ) (t : b_typ) (k : kind) : bool =
  match k with
  | KStar ->
    begin match path_equiv g (weak_norm s) (weak_norm t) with
      | Some KStar -> true
      | _ -> false
    end
  | KArrow (k1, k2) ->
    typ_equiv
      (k1 :: g)
      (TApp (rename_typ ((+) 1) s, TVar 0))
      (TApp (rename_typ ((+) 1) t, TVar 0)) k2
      
and path_equiv (g : kind list) (p : b_typ) (q : b_typ) : kind option =
  match p, q with
  | TBot, TBot -> some KStar
  | TVar m, TVar n when m = n -> List.nth g m
  | TApp (p, s), TApp (q, t) ->
    path_equiv g p q >>= fun k ->
    begin match k with
      | KArrow (k1, k2) when typ_equiv g s t k1 -> some k2
      | _ -> None
    end
  | TArrow (p1, p2), TArrow (q1, q2) ->
    path_equiv g p1 q1 >>=
    begin function
      | KStar -> path_equiv g p2 q2 >>=
        begin function
          | KStar -> some KStar
          | _ -> None
        end
      | _ -> None
    end
  | _ -> None
*)    
