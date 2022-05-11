open Core
open Util
open FunUtil
open CompUtil
open Syntax

let rec rename_typ (r : int -> int) : b_typ -> b_typ =
  function
  | TVar x -> TVar (r x)
  | TForall (_,t) -> TForall ((), rename_typ (ext r) t)
  | TArrow (t1,t2) -> TArrow (rename_typ r t1, rename_typ r t2)

let exts_typ (s : int -> b_typ) (x : int) : b_typ =
  if x < 1 then TVar x else rename_typ ((+) 1) $ s (x - 1)

let rec subs_typ (s : int -> b_typ) : b_typ -> b_typ =
  function
  | TVar x -> s x
  | TForall (_,t) -> TForall ((), subs_typ (exts_typ s) t)
  | TArrow (t1,t2) -> TArrow (subs_typ s t1, subs_typ s t2)

let sub_typ_helper (t : b_typ) (x : int) : b_typ =
  if x = 0 then t else TVar (x - 1)

let sub_typ ~arg:(arg : b_typ) : b_typ -> b_typ =
  subs_typ $ sub_typ_helper arg

let rec rename_typ_term (r : int -> int) : b_expr -> b_expr =
  function
  | Var x -> Var x
  | Abs (_,t,e) -> Abs ((), rename_typ r t, rename_typ_term r e)
  | App (e1,e2) -> App (rename_typ_term r e1, rename_typ_term r e2)
  | TypAbs (_,e) -> TypAbs ((), rename_typ_term (ext r) e)
  | TypApp (e,t) -> TypApp (rename_typ_term r e, rename_typ r t)

let rec subs_typ_term (s : int -> b_typ) : b_expr -> b_expr =
  function
  | Var x -> Var x
  | Abs (_,t,e) -> Abs ((), subs_typ s t, subs_typ_term s e)
  | App (e1,e2) -> App (subs_typ_term s e1, subs_typ_term s e2)
  | TypAbs (_,e) -> TypAbs ((), subs_typ_term (exts_typ s) e)
  | TypApp (e,t) -> TypApp (subs_typ_term s e, subs_typ s t)

let sub_typ_term (e : b_expr) (t : b_typ) : b_expr =
  subs_typ_term (sub_typ_helper t) e

let rec rename (r : int -> int) : b_expr -> b_expr =
  function
  | Var x -> Var (r x)
  | Abs (_,t,e) -> Abs ((), t, rename (ext r) e)
  | App (e1,e2) -> App (rename r e1, rename r e2)
  | TypAbs (_,e) -> TypAbs ((), rename r e)
  | TypApp (e,t) -> TypApp (rename r e, t)

let exts (s : int -> b_expr) (x : int) : b_expr =
  if x < 1 then Var x else rename ((+) 1) $ s (x - 1)

let rec subs (s : int -> b_expr) : b_expr -> b_expr =
  function
  | Var x -> s x
  | Abs (_,t,e) -> Abs ((), t, subs (exts s) e)
  | App (e1,e2) -> App (subs s e1, subs s e2)
  | TypAbs (_,e) -> TypAbs ((), subs s e)
  | TypApp (e,t) -> TypApp (subs s e, t)

let sub_helper (e : b_expr) (x : int) : b_expr =
  if x = 0 then e else Var (x - 1)

(** Beta-reduction of [(fun x => e1) e2 -> e1{e2/x}] *)
let sub ~arg:(arg : b_expr) : b_expr -> b_expr =
  subs $ sub_helper arg
