open Syntax

let rec shift_typ (c: int) (i: int)
  : b_typ -> b_typ = function
  | TVar n ->
    TVar (if n < c then n else n + i)
  | TForall (_,t) ->
    TForall ((), shift_typ (c + 1) i t)
  | TArrow (t1,t2) ->
    TArrow (shift_typ c i t1, shift_typ c i t2)

let rec shift_expr
    (ct: int) (ce: int) (it: int) (ie: int)
  : b_expr -> b_expr = function
  | Var n ->
    Var (if n < ce then n else n + ie)
  | Abs (_,t,e) ->
    Abs ((), shift_typ ct it t,
         shift_expr ct (ce + 1) it ie e)
  | App (e1,e2) ->
    App (shift_expr ct ce it ie e1,
         shift_expr ct ce it ie e2)
  | TypAbs (_,e) ->
    TypAbs ((), shift_expr (ct + 1) ce it ie e)
  | TypApp (e,t) ->
    TypApp (shift_expr ct ce it ie e,
            shift_typ ct it t)

(** Substituting a [typ] into a [typ]. *)
let rec typ_sub (n: int) (ts: b_typ)
  : b_typ -> b_typ = function
  | TVar m ->
    if n = m then shift_typ 0 n ts else TVar m
  | TForall (_,t) ->
    TForall ((), typ_sub (n + 1) ts t)
  | TArrow (t1,t2) ->
    TArrow (typ_sub n ts t1, typ_sub n ts t2)

(** Substituting a [expr] into a [expr]. *)
let rec expr_sub (n: int) (es: b_expr)
  : b_expr -> b_expr = function
  | Var m ->
    if n = m then shift_expr 0 0 0 n es else Var m
  | Abs (_,t,e) ->
    Abs ((), t, expr_sub (n + 1) es e)
  | App (e1,e2) ->
    App (expr_sub n es e1, expr_sub n es e2)
  | TypAbs (_,e) ->
    TypAbs ((), expr_sub n es e)
  | TypApp (e,t) ->
    TypApp (expr_sub n es e, t)

(** Substituting a [typ] into a [expr]. *)
let rec typ_expr_sub (n: int) (ts: b_typ)
  : b_expr -> b_expr = function
  | Var m -> Var m
  | Abs (_,t,e) ->
    Abs ((), typ_sub n ts t, typ_expr_sub n ts e)
  | App (e1,e2) ->
    App (typ_expr_sub n ts e1, typ_expr_sub n ts e2)
  | TypAbs (_,e) ->
    TypAbs ((), typ_expr_sub (n + 1) ts e)
  | TypApp (e,t) ->
    TypApp (typ_expr_sub n ts e, typ_sub n ts t)

(** Top-level functions. *)
