open Core
open Option
open Syntax
open Util
open FunUtil

(** A Pure Type System is parameterized
    by a triple (S,A,R):
    by a set of sorts S,
    axioms A of the form s1 : s2,
    and rules R of the form (s1,s2,s3).

    In the general the set of sorts
    may be an infinitely ascending hierarchy,
    so my implemtation uses [sort] in syntax.ml
    forall triples.

    The axioms specify the typing of sorts.
    s1 : s2 denotes sort s1 is typed as sort s2.

    The rules specify the typing of pi-types,
    i.e. terms of the form [∏ x:t1. t2].
    The literature refers to these as dependent products.
    (Personally this is confusing so I use "pi-type" instead.)
    A rule (s1,s2,s3) allows pi-types such that
    
    G |- t1 : s1   G, x:t1 |- t2 : s2
    ----------------------------------∏
           G |- ∏ x:t1. t2 : s3

    In many pure type systems s3 = s2
    (the whole pi-type has the same type as [t2]),
    and when this is the case the rule (s1,s2)
    is written in place of (s1,s2,s2). *)
module type Triple = sig
  (** [axioms s1 = Some s2] denotes [g |- s1 : s2]. *)
  val axioms : sort -> sort option

  (** [rules s1 s2 = Some s3] [g |- Π t1. t2 : s3]
      such that [g |- t1 : s1] & [t1 :: g |- t2 : s2] .*)
  val rules : sort -> sort -> sort option
end

let cube_axioms : sort -> sort option = function
  | Prop -> some $ Suc Prop
  | _ -> None

let u_axioms : sort -> sort option = function
  | Prop -> some $ Suc Prop
  | Suc Prop -> some $ Suc (Suc Prop)
  | _ -> None

(** Simply-typed lambda-calculus:
    Axiom: * : □.
    Rule: ( * , * ). *)
module STLC : Triple = struct
  let axioms = cube_axioms
  
  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop -> some Prop
    | _, _ -> None
end

(** System F:
    Axiom: * : □.
    Rules: ( * , * ) ( □ , * ). *)
module SystemF : Triple = struct
  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Suc Prop, Prop -> some Prop
    | _, _ -> None
end

(** Lambda Omgea:
    Axiom: * : □.
    Rules: ( * , * ) ( □ , □ ). *)
module LambdaOmgea : Triple = struct
  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Suc Prop, Suc Prop -> some s2
    | _, _ -> None
end

(** System F-Omega:
    Axiom: * : □.
    Rules:  ( *  , *  ) ( □ , * ) ( □ , □ ). *)
module SysFOmega : Triple = struct
  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Suc Prop, Prop
    | Suc Prop, Suc Prop -> some s2
    | _, _ -> None
end

(** Lambda P:
    Axiom: * : □.
    Rules: ( * , * ) ( * , □ ). *)
module LambdaP : Triple = struct
  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Prop, Suc Prop -> some s2
    | _, _ -> None
end

(** Lambda P and System F:
    Axiom: * : □.
    Rules: ( * , * ) ( * , □ ) ( □ , * ). *)
module LambdaP_SysF : Triple = struct
  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Prop, Suc Prop
    | Suc Prop, Prop -> some s2
    | _, _ -> None
end

(** Lambda P and Omega:
    Axiom: * : □.
    Rules: ( * , * ) ( * , □ ) ( □ , □ ). *)
module LambdaP_Omega : Triple = struct
  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Prop, Suc Prop
    | Suc Prop, Suc Prop -> some s2
    | _, _ -> None
end

(** Calculus of constructions:
    Axiom: * : □.
    Rules: ( * , * ) ( * , □ ) ( □ , * ) ( □ , □ ). *)
module COC : Triple = struct
  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Prop, Suc Prop
    | Suc Prop, Prop
    | Suc Prop, Suc Prop -> some s2
    | _, _ -> None
end

(** Lambda HOL:
    Axioms: * : □, □ : ∆.
    Rules: ( * , * ) ( □ , * ) ( □ , □ ). *)
module Lambda_HOL : Triple = struct
  let axioms = u_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Prop, Suc Prop
    | Suc Prop, Suc Prop -> some s2
    | _, _ -> None
end

(** Lambda HOL Extended:
    Axioms: * : □, □ : ∆.
    Rules: ( * , * ) ( □ , * ) ( □ , □ ) ( ∆ , * ). *)
module Lambda_HOL_Extended : Triple = struct
  let axioms = u_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Prop, Suc Prop
    | Suc Prop, Suc Prop
    | Suc (Suc Prop), Prop -> some s2
    | _, _ -> None
end

(** System U Minus:
    Axioms: * : □, □ : ∆.
    Rules: ( * , * ) ( □ , * ) ( □ , □ ) ( ∆ , □ ). *)
module System_U_Minus : Triple = struct
  let axioms = u_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Prop, Suc Prop
    | Suc Prop, Suc Prop
    | Suc (Suc Prop), Suc Prop -> some s2
    | _, _ -> None
end

(** System U:
    Axioms: * : □, □ : ∆.
    Rules: ( * , * ) ( □ , * ) ( □ , □ ) ( ∆ , * ) ( ∆ , □ ). *)
module SystemU : Triple = struct
  let axioms = u_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Suc Prop, Prop
    | Suc Prop, Suc Prop
    | Suc (Suc Prop), Prop
    | Suc (Suc Prop), Suc Prop -> some s2
    | _, _ -> None
end
