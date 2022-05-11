open Core
open Util
open FunUtil
open CompUtil
open Syntax

(** Shifting & Substitution. *)

let rec rename_typ (r : int -> int) : b_typ -> b_typ =
  function
  | TBot -> TBot
  | TVar x -> TVar (r x)
  | TAbs (_,k,t) -> TAbs ((), k, rename_typ (ext r) t)
  | TApp (t1,t2) -> TApp (rename_typ r t1, rename_typ r t2)
  | TArrow (t1,t2) -> TArrow (rename_typ r t1, rename_typ r t2)

let exts_typ (s : int -> b_typ) (x : int) : b_typ =
  if x < 1 then TVar x else rename_typ ((+) 1) $ s (x - 1)

let rec subs_typ (s : int -> b_typ) : b_typ -> b_typ =
  function
  | TBot -> TBot
  | TVar x -> s x
  | TAbs (_,k,t) -> TAbs ((), k, subs_typ (exts_typ s) t)
  | TApp (t1,t2) -> TApp (subs_typ s t1, subs_typ s t2)
  | TArrow (t1,t2) -> TArrow (subs_typ s t1, subs_typ s t2)

let sub_typ_helper (t : b_typ) (x : int) : b_typ =
  if x = 0 then t else TVar (x - 1)

let sub_typ ~arg:(arg : b_typ) : b_typ -> b_typ =
  subs_typ $ sub_typ_helper arg

let rec rename (r : int -> int) : b_term -> b_term =
  function
  | Var x -> Var (r x)
  | Abs (_,t,e) -> Abs ((), t, rename (ext r) e)
  | App (e1,e2) -> App (rename r e1, rename r e2)

let exts (s : int -> b_term) (x : int) : b_term =
  if x < 1 then Var x else rename ((+) 1) $ s (x - 1)

let rec subs (s : int -> b_term) : b_term -> b_term =
  function
  | Var x -> s x
  | Abs (_,t,e) -> Abs ((), t, subs (exts s) e)
  | App (e1,e2) -> App (subs s e1, subs s e2)

let sub_helper (e : b_term) (x : int) : b_term =
  if x = 0 then e else Var (x - 1)

(** Beta-reduction of [(fun x => e1) e2 -> e1{e2/x}] *)
let sub ~arg:(arg : b_term) : b_term -> b_term =
  subs $ sub_helper arg
