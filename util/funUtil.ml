open Core

(** Identity function. *)
let id (a: 'a) : 'a = a

(** Function composition. *)
let (>>) f g = fun x -> x |> g |> f

(** Haskell-style dollar-sign *)
let ($) f x = f x

let my_ignore : 'a. 'a -> unit = fun _ -> ()

(** Consume unused argument. *)
let consume (f: 'b) : 'a -> 'b = fun _ -> f

(** Apply a function [n] times. *)
let rec napply (n: int) (f: 'a -> 'a) (a: 'a) : 'a =
  if n <= 0 then a else f $ napply (n-1) f a

(** Multi-reduction. *)
let rec multi_red
    (red : 'a -> 'a option)
    (f : 'a -> unit) (e : 'a) : 'a option =
  let open Option in
  f e; red e >>= multi_red red f

let rec refl_trans_clos
  : 'a. ('a -> 'a option) -> ('a -> unit) -> 'a -> 'a =
  fun red f t ->
  f t; match red t with
  | Some t -> refl_trans_clos red f t
  | None -> t

let switch (f: 'a -> 'b -> 'c) (b: 'b) (a: 'a) : 'c = f a b
