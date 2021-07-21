open Core
open Util
open FunUtil
open CompUtil
open IntComp
open Syntax

let shift_typ (c: int) (i: int)
  : b_typ -> b_typ =
  typ_map_ctx
    ~ctx:c ~succ:(consume $ (+) 1)
    ~f:(fun c n -> if n < c then n else n + i)
    ~g:(consume my_ignore)

let shift_expr
    (ct: int) (ce: int) (it: int) (ie: int)
  : b_expr -> b_expr =
  expr_map_ctx
    ~tctx:ct ~ectx:ce ~succ:(consume $ (+) 1)
    ~f:(fun ct n -> if n < ct then n else n + it)
    ~g:(consume my_ignore)
    ~h:(fun _ ce n -> if n < ce then n else n + ie)
    ~i:(consume my_ignore)

(** Substituting a [typ] into a [typ]. *)
let typ_sub (n: int) (ts: b_typ)
  : b_typ -> b_typ =
  typ_scheme
    ~ctx:n ~succ:(consume $ (+) 1)
    ~f:(fun n m ->
        match n <=> m with
        | LT -> tvar $ m - 1
        | EQ -> shift_typ 0 n ts
        | GT -> tvar m)
    ~g:(consume my_ignore)

(** Substituting a [expr] into a [expr]. *)
let expr_sub (td: int) (n: int) (es: b_expr)
  : b_expr -> b_expr =
  expr_scheme
    ~tctx:td ~ectx:n ~succ:(consume $ (+) 1)
    ~f:(consume tvar)
    ~g:(consume my_ignore)
    ~h:(fun td n m ->
        match n <=> m with
        | LT -> var $ m - 1
        | EQ -> shift_expr 0 0 td n es
        | GT -> var m)
    ~i:(consume my_ignore)

(** Substituting a [typ] into a [expr]. *)
let typ_expr_sub (n: int) (ts: b_typ)
  : b_expr -> b_expr =
  expr_scheme
    ~tctx:n ~ectx:0 ~succ:(consume $ (+) 1)
    ~f:(fun n m ->
        match n <=> m with
        | LT -> tvar $ m - 1
        | EQ -> shift_typ 0 n ts
        | GT -> tvar m)
    ~g:(consume my_ignore)
    ~h:(consume $ consume var)
    ~i:(consume my_ignore)
