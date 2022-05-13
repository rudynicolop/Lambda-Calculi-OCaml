open Core
open Option

module type Triple = sig
  (** Available sorts. *)
  type sort

  (** [axioms s1 = Some s2] denotes [g |- s1 : s2]. *)
  val axioms : sort -> sort option

  (** [rules s1 s2 = true] allows [Π t1. t2]
      such that [g |- t1 : s1] & [g |- t2 : s2] .*)
  val rules : sort -> sort -> bool

  (** Sort equality. *)
  val (=?) : sort -> sort -> bool

  (** For printing. *)
  val string_of_sort : sort -> string
end

(** Sorts of the Lambda-Cube. *)
type cube_sort = Type | Kind

(** Axioms of the Lambda-Cube. *)
let cube_axioms = function
    | Type -> some Kind
    | Kind -> None

let cube_sort_eq s1 s2 =
  match s1, s2 with
  | Type, Type
  | Kind, Kind -> true
  | _, _ -> false

let string_of_cube_sort = function
  | Type -> "*"
  | Kind -> "□"

(** Simply-typed lambda-calculus. *)
module STLC : Triple = struct
  type sort = cube_sort

  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Type, Type -> true
    | _, _ -> false

  let (=?) = cube_sort_eq

  let string_of_sort = string_of_cube_sort
end

(** System F. *)
module SystemF : Triple = struct
  type sort = cube_sort

  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Type, Type
    | Kind, Type -> true
    | _, _ -> false

  let (=?) = cube_sort_eq

  let string_of_sort = string_of_cube_sort
end

(** Lambda Omgea. *)
module LambdaOmgea : Triple = struct
  type sort = cube_sort

  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Type, Type
    | Kind, Kind -> true
    | _, _ -> false

  let (=?) = cube_sort_eq

  let string_of_sort = string_of_cube_sort
end

(** System F-Omega. *)
module SysFOmega : Triple = struct
  type sort = cube_sort

  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Type, Type
    | Kind, Type
    | Kind, Kind -> true
    | _, _ -> false

  let (=?) = cube_sort_eq

  let string_of_sort = string_of_cube_sort
end

(** Lambda P. *)
module LambdaP : Triple = struct
  type sort = cube_sort

  let axioms = cube_axioms

  let rules s1 s2 =
    match s1, s2 with
    | Type, Type
    | Type, Kind -> true
    | _, _ -> false

  let (=?) = cube_sort_eq

  let string_of_sort = string_of_cube_sort
end

(** Calculus of constructions. *)
module COC : Triple = struct
  type sort = cube_sort
              
  let axioms = cube_axioms

  (** All combinations allowed. *)
  let rules _ _ = true

  let (=?) = cube_sort_eq

  let string_of_sort = string_of_cube_sort
end

(** System U. *)
module SystemU : Triple = struct
  type sort = Star | Square | Triangle

  let _ = Star
  
  let axioms = function
    | Star -> some Square
    | Square -> some Triangle
    | Triangle -> None

  let rules s1 s2 =
    match s1, s2 with
    | Star, Star
    | Square, Star
    | Square, Square
    | Triangle, Star
    | Triangle, Square -> true
    | _, _ -> false

  let (=?) s1 s2 =
    match s1, s2 with
    | Star, Star
    | Square, Square
    | Triangle, Triangle -> true
    | _, _ -> false

  let string_of_sort = function
    | Star -> "*"
    | Square -> "□"
    | Triangle -> "∆"
end
