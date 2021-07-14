open Util
open FunUtil
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
    ~f:(fun n m -> if n = m then shift_typ 0 n ts else TVar m)
    ~g:(consume my_ignore)

(** Substituting a [expr] into a [expr]. *)
let expr_sub (td: int) (n: int) (es: b_expr)
  : b_expr -> b_expr =
  expr_scheme
    ~tctx:td ~ectx:n ~succ:(consume $ (+) 1)
    ~f:(consume tvar)
    ~g:(consume my_ignore)
    ~h:(fun td n m -> if n = m then shift_expr 0 td 0 n es else Var m)
    ~i:(consume my_ignore)

(** Substituting a [typ] into a [expr]. *)
let typ_expr_sub (n: int) (ts: b_typ)
  : b_expr -> b_expr =
  expr_scheme
    ~tctx:n ~ectx:0 ~succ:(consume $ (+) 1)
    ~f:(fun n m -> if n = m then shift_typ 0 n ts else TVar m)
    ~g:(consume my_ignore)
    ~h:(consume $ consume var)
    ~i:(consume my_ignore)

(** Top-level functions. *)

let typ_sub_top (ts: b_typ) (t: b_typ) : b_typ =
  shift_typ 0 (-1) $ typ_sub 0 (shift_typ 0 1 ts) t

let expr_sub_top (es: b_expr) (e: b_expr) : b_expr =
  shift_expr 0 0 0 (-1) $ expr_sub 0 0 (shift_expr 0 0 0 1 es) e

let typ_expr_sub_top (ts: b_typ) (e: b_expr) : b_expr =
  shift_expr 0 0 (-1) 0 $ typ_expr_sub 0 (shift_typ 0 1 ts) e
