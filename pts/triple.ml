open Core
open Option
open Syntax
open Util
open FunUtil

module type Triple = sig
  (** [axioms s1 = Some s2] denotes [g |- s1 : s2]. *)
  val axioms : sort -> sort option

  (** [rules s1 s2 = true] [g |- Π t1. t2 : s3]
      such that [g |- t1 : s1] & [g |- t2 : s2] .*)
  val rules : sort -> sort -> sort option
end

let cube_axioms : sort -> sort option = function
  | Prop -> some $ Suc Prop
  | _ -> None

(** Simply-typed lambda-calculus. *)
module STLC : Triple = struct
  let axioms = cube_axioms
  
  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop -> some Prop
    | _, _ -> None
end

(** System F. *)
module SystemF : Triple = struct
  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Suc Prop, Prop -> some Prop
    | _, _ -> None
end

(** Lambda Omgea. *)
module LambdaOmgea : Triple = struct
  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Suc Prop, Suc Prop -> some s2
    | _, _ -> None
end

(** System F-Omega. *)
module SysFOmega : Triple = struct
  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Suc Prop, Prop
    | Suc Prop, Suc Prop -> some s2
    | _, _ -> None
end

(** Lambda P. *)
module LambdaP : Triple = struct
  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Prop, Suc Prop -> some s2
    | _, _ -> None
end

(** Calculus of constructions. *)
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

(** System U. *)
module SystemU : Triple = struct
  let axioms = function
    | Prop -> some $ Suc Prop
    | Suc Prop -> some $ Suc (Suc Prop)
    | _ -> None

  let rules s1 s2 =
    match s1, s2 with
    | Prop, Prop
    | Suc Prop, Prop
    | Suc Prop, Suc Prop
    | Suc (Suc Prop), Prop
    | Suc (Suc Prop), Suc Prop -> some s2
    | _, _ -> None
end
