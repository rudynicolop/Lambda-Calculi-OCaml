open Core
open Util
open CompUtil
open IntComp
open FunUtil
open Syntax
open Fold

(** Index shifting. *)

let shift_typ (i: int) (c: int) : b_typ -> b_typ =
  map_typ_ctx
    ~ctx:c ~f:(consume $ (+) 1)
    ~var:(fun c n -> if n < c then n else n + i)
    ~abs:(consume my_ignore) ~all:(consume my_ignore)

let shift_term
    (it: int) (i: int) (ct: int) (c: int) : b_term -> b_term =
  map_term_ctx
    ~tctx:ct ~ctx:c
    ~f:(consume $ (+) 1) ~ty:(shift_typ it)
    ~var:(fun c n -> if n < c then n else n + i)
    ~abs:(consume my_ignore) ~tabs:(consume my_ignore)

(** Substitution. *)

let sub_typ (n: int) (ts: b_typ) : b_typ -> b_typ =
  typ_scheme
    ~ctx:n ~f:(consume $ (+) 1)
    ~var:(fun n m ->
        match n <=> m with
        | LT -> tvar $ m - 1
        | EQ -> shift_typ n 0 ts
        | GT -> tvar m)
    ~abs:(consume my_ignore) ~all:(consume my_ignore)

let sub_term (td: int) (n: int) (es: b_term) : b_term -> b_term =
  term_scheme
    ~tctx:td ~ctx:n ~f:(consume $ (+) 1) ~ty:(consume $ id)
    ~var:(fun n m ->
        match n <=> m with
        | LT -> var $ m - 1
        | EQ -> shift_term td n 0 0 es
        | GT -> var m)
    ~abs:(consume my_ignore) ~tabs:(consume my_ignore)

let sub_typ_term (td: int) (ts: b_typ) : b_term -> b_term =
  map_term_ctx
    ~tctx:td ~ctx:0 ~f:(consume $ (+) 1)
    ~ty:(fun td -> sub_typ td ts)
    ~var:(consume id)
    ~abs:(consume id) ~tabs:(consume id)
