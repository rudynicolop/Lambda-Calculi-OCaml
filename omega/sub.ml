open Core
open Util
open FunUtil
open Syntax
open Fold

(** Shifting & Substitution. *)

(** Shifting. *)

let shift_typ
    (c: int) (i: int) : b_typ -> b_typ =
  map_typ_ctx
    ~ctx:c ~succ:(consume $ (+) 1)
    ~var:(fun c n -> if n < c then n else n + i)
    ~abs:(consume my_ignore)

let shift_term
    (c: int) (i: int) : b_term -> b_term =
  map_term_ctx
    ~ctx:c ~succ:(consume $ (+) 1) ~ty:id
    ~var:(fun c n -> if n < c then n else n + i)
    ~abs:(consume my_ignore)

(** Substitution. *)

let sub_typ
    (n: int) (ts: b_typ) : b_typ -> b_typ =
  typ_scheme
    ~ctx:n ~succ:(consume $ (+) 1)
    ~var:(fun n m -> if n = m then ts else tvar m)
    ~abs:(consume my_ignore)

let sub_term
    (n: int) (es: b_term) : b_term -> b_term =
  term_scheme
    ~ctx:n ~succ:(consume $ (+) 1)
    ~var:(fun n m -> if n = m then es else var m)
    ~ty:id ~abs:(consume my_ignore)

(** Top-level functions. *)

let sub_typ_top (ts: b_typ) (t: b_typ) : b_typ =
  shift_typ 0 1 $ sub_typ 0 (shift_typ 0 (-1) ts) t

let sub_term_top (ts: b_term) (t: b_term) : b_term =
  shift_term 0 1 $ sub_term 0 (shift_term 0 (-1) ts) t
