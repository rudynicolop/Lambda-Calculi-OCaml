open Core
open Util
open FunUtil
open CompUtil
open IntComp
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
    ~var:(fun n m ->
        match n <=> m with
        | LT -> tvar $ m - 1
        | EQ -> shift_typ 0 n ts
        | GT -> tvar m)
    ~abs:(consume my_ignore)

let sub_term
    (n: int) (es: b_term) : b_term -> b_term =
  term_scheme
    ~ctx:n ~succ:(consume $ (+) 1)
    ~var:(fun n m ->
        match n <=> m with
        | LT -> var $ m - 1
        | EQ -> shift_term 0 n es
        | GT -> var m)
    ~ty:id ~abs:(consume my_ignore)
